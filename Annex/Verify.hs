{- verification
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Verify (
	VerifyConfig(..),
	shouldVerify,
	verifyKeyContentPostRetrieval,
	verifyKeyContent,
	Verification(..),
	unVerified,
	warnUnverifiableInsecure,
	isVerifiable,
	startVerifyKeyContentIncrementally,
	IncrementalVerifier(..),
	tailVerify,
) where

import Annex.Common
import qualified Annex
import qualified Types.Remote
import qualified Types.Backend
import Types.Backend (IncrementalVerifier(..))
import qualified Backend
import Types.Remote (unVerified, Verification(..), RetrievalSecurityPolicy(..))
import Annex.WorkerPool
import Types.WorkerPool
import Types.Key

#if WITH_INOTIFY
import qualified System.INotify as INotify
import Control.Concurrent.STM
import qualified Data.ByteString as S
#endif

data VerifyConfig = AlwaysVerify | NoVerify | RemoteVerify Remote | DefaultVerify

shouldVerify :: VerifyConfig -> Annex Bool
shouldVerify AlwaysVerify = return True
shouldVerify NoVerify = return False
shouldVerify DefaultVerify = annexVerify <$> Annex.getGitConfig
shouldVerify (RemoteVerify r) = 
	(shouldVerify DefaultVerify
			<&&> pure (remoteAnnexVerify (Types.Remote.gitconfig r)))
	-- Export remotes are not key/value stores, so always verify
	-- content from them even when verification is disabled.
	<||> Types.Remote.isExportSupported r

{- Verifies that a file is the expected content of a key.
 -
 - Configuration can prevent verification, for either a
 - particular remote or always, unless the RetrievalSecurityPolicy
 - requires verification.
 -
 - Most keys have a known size, and if so, the file size is checked.
 -
 - When the key's backend allows verifying the content (via checksum),
 - it is checked. 
 -
 - If the RetrievalSecurityPolicy requires verification and the key's
 - backend doesn't support it, the verification will fail.
 -}
verifyKeyContentPostRetrieval :: RetrievalSecurityPolicy -> VerifyConfig -> Verification -> Key -> RawFilePath -> Annex Bool
verifyKeyContentPostRetrieval rsp v verification k f = case (rsp, verification) of
	(_, Verified) -> return True
	(RetrievalVerifiableKeysSecure, _) -> ifM (isVerifiable k)
		( verify
		, ifM (annexAllowUnverifiedDownloads <$> Annex.getGitConfig)
			( verify
			, warnUnverifiableInsecure k >> return False
			)
		)
	(_, UnVerified) -> ifM (shouldVerify v)
		( verify
		, return True
		)
	(_, MustVerify) -> verify
  where
	verify = enteringStage VerifyStage $ verifyKeyContent k f

verifyKeyContent :: Key -> RawFilePath -> Annex Bool
verifyKeyContent k f = verifysize <&&> verifycontent
  where
	verifysize = case fromKey keySize k of
		Nothing -> return True
		Just size -> do
			size' <- liftIO $ catchDefaultIO 0 $ getFileSize f
			return (size' == size)
	verifycontent = Backend.maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
		Nothing -> return True
		Just b -> case Types.Backend.verifyKeyContent b of
			Nothing -> return True
			Just verifier -> verifier k f

warnUnverifiableInsecure :: Key -> Annex ()
warnUnverifiableInsecure k = warning $ unwords
	[ "Getting " ++ kv ++ " keys with this remote is not secure;"
	, "the content cannot be verified to be correct."
	, "(Use annex.security.allow-unverified-downloads to bypass"
	, "this safety check.)"
	]
  where
	kv = decodeBS (formatKeyVariety (fromKey keyVariety k))

isVerifiable :: Key -> Annex Bool
isVerifiable k = maybe False (isJust . Types.Backend.verifyKeyContent) 
	<$> Backend.maybeLookupBackendVariety (fromKey keyVariety k)

startVerifyKeyContentIncrementally :: VerifyConfig -> Key -> Annex (Maybe IncrementalVerifier)
startVerifyKeyContentIncrementally verifyconfig k = 
	ifM (shouldVerify verifyconfig)
		( Backend.maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
			Just b -> case Types.Backend.verifyKeyContentIncrementally b of
				Just v -> Just <$> v k
				Nothing -> return Nothing
			Nothing -> return Nothing
		, return Nothing
		)

-- | Reads the file as it grows, and feeds it to the incremental verifier.
-- 
-- The TMVar must start out empty, and be filled once whatever is
-- writing to the file finishes.
--
-- The file does not need to exist yet when this is called. It will wait
-- for the file to appear before opening it and starting verification.
--
-- Note that there are situations where the file may fail to verify despite
-- having the correct content. For example, when the file is written out
-- of order, or gets replaced part way through. To deal with such cases,
-- when False is returned, it should not be treated as if the file's
-- content is known to be incorrect, but instead as an indication that the
-- file should be verified again, once it's done being written to.
--
-- Also, this is not supported for all OSs, and will return False on OS's
-- where it is not supported.
--
-- This should be fairly efficient, reading from the disk cache,
-- as long as the writer does not get very far ahead of it. However,
-- there are situations where it would be much less expensive to verify
-- chunks as they are being written. For example, when resuming with
-- a lot of content in the file, all that content needs to be read,
-- and if the disk is slow, the reader may never catch up to the writer,
-- and the disk cache may never speed up reads. So this should only be
-- used when there's not a better way to incrementally verify.
tailVerify :: IncrementalVerifier -> FilePath -> TMVar () -> IO Bool
#if WITH_INOTIFY
tailVerify iv f finished = 
	catchDefaultIO False $ INotify.withINotify $ \i ->
		bracket (waitforfiletoexist i) (maybe noop hClose) (go i)
  where
 	rf = toRawFilePath f
	d = toRawFilePath (takeDirectory f)

	waitforfiletoexist i = tryIO (openBinaryFile f ReadMode) >>= \case
		Right h -> return (Just h)
		Left _ -> do
			hv <- newEmptyTMVarIO
			wd <- inotifycreate i $
				tryIO (openBinaryFile f ReadMode) >>= \case
					Right h -> 
						unlessM (atomically $ tryPutTMVar hv h) $
							hClose h
					Left _ -> return ()
			-- Wait for the file to appear, or for a signal
			-- that the file is finished being written.
			--
			-- The TMVar is left full to prevent the file
			-- being opened again if the inotify event
			-- fires more than once.
			v <- atomically $
				(Just <$> readTMVar hv)
					`orElse` 
				((const Nothing) <$> readTMVar finished)
			INotify.removeWatch wd
			return v
	
	inotifycreate i cont = INotify.addWatch i evs d $ \case
		-- Ignore changes to other files in the directory.
		INotify.Created { INotify.filePath = fn }
			| fn /= rf -> noop
		INotify.MovedIn { INotify.filePath = fn }
			| fn /= rf -> noop
		INotify.Opened { INotify.maybeFilePath = fn }
			| fn /= Just rf -> noop
		INotify.Modified { INotify.maybeFilePath = fn }
			| fn /= Just rf -> noop
		_ -> cont
	  where
		evs = 
			[ INotify.Create
			, INotify.MoveIn
			, INotify.Move
			, INotify.Open
			, INotify.Modify
			]
	
	go i (Just h) = do
		modified <- newEmptyTMVarIO
		wd <- INotify.addWatch i [INotify.Modify] rf $ \_event ->
			atomically $ void $ tryPutTMVar modified ()
		r <- follow h modified
		INotify.removeWatch wd
		return r
	-- File never showed up, but we've been told it's done being
	-- written to.
	go _ Nothing = return False
	
	follow h modified = do
		b <- S.hGetNonBlocking h chunk
		if S.null b
			then do
				-- We've caught up to the writer.
				-- Wait for the file to get modified again,
				-- or until we're told it is done being
				-- written.
				cont <- atomically $
					((const (follow h modified))
						<$> takeTMVar modified)
							`orElse`
					((const (finish h))
						<$> readTMVar finished)
				cont
			else do
				updateIncremental iv b
				follow h modified
	
	-- We've been told the file is done being written to, but we
	-- may not have reached the end of it yet. Read until EOF.
	finish h = do
		b <- S.hGet h chunk
		if S.null b
			then finalizeIncremental iv
			else do
				updateIncremental iv b
				finish h
	

	chunk = 65536
#else
tailVerify _ _ _ = return False -- not supported
#endif

