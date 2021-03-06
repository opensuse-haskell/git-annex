{- helpers for special remotes
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Helper.Special (
	findSpecialRemotes,
	gitConfigSpecialRemote,
	mkRetrievalVerifiableKeysSecure,
	Storer,
	Retriever,
	Remover,
	CheckPresent,
	ContentSource,
	fileStorer,
	byteStorer,
	fileRetriever,
	byteRetriever,
	storeKeyDummy,
	retrieveKeyFileDummy,
	removeKeyDummy,
	checkPresentDummy,
	SpecialRemoteCfg(..),
	specialRemoteCfg,
	specialRemoteConfigParsers,
	specialRemoteType,
	specialRemote,
	specialRemote',
	lookupName,
	module X
) where

import Annex.Common
import Annex.SpecialRemote.Config
import Types.StoreRetrieve
import Types.Remote
import Annex.UUID
import Config
import Config.Cost
import Utility.Metered
import Remote.Helper.Chunked as X
import Remote.Helper.Encryptable as X
import Annex.Content
import Messages.Progress
import qualified Git
import qualified Git.Construct
import Git.Types

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

{- Special remotes don't have a configured url, so Git.Repo does not
 - automatically generate remotes for them. This looks for a different
 - configuration key instead.
 -}
findSpecialRemotes :: String -> Annex [Git.Repo]
findSpecialRemotes s = do
	m <- fromRepo Git.config
	liftIO $ catMaybes <$> mapM construct (remotepairs m)
  where
	remotepairs = M.toList . M.filterWithKey match
	construct (k,_) = Git.Construct.remoteNamedFromKey k
		(pure Git.Construct.fromUnknown)
	match (ConfigKey k) _ =
		"remote." `S.isPrefixOf` k 
		&& (".annex-" <> encodeBS' s) `S.isSuffixOf` k

{- Sets up configuration for a special remote in .git/config. -}
gitConfigSpecialRemote :: UUID -> RemoteConfig -> [(String, String)] -> Annex ()
gitConfigSpecialRemote u c cfgs = do
	forM_ cfgs $ \(k, v) -> 
		setConfig (remoteAnnexConfig c (encodeBS' k)) v
	storeUUIDIn (remoteAnnexConfig c "uuid") u

-- RetrievalVerifiableKeysSecure unless overridden by git config.
--
-- Only looks at the RemoteGitConfig; the GitConfig's setting is
-- checked at the same place the RetrievalSecurityPolicy is checked.
mkRetrievalVerifiableKeysSecure :: RemoteGitConfig -> RetrievalSecurityPolicy
mkRetrievalVerifiableKeysSecure gc
	| remoteAnnexAllowUnverifiedDownloads gc = RetrievalAllKeysSecure
	| otherwise = RetrievalVerifiableKeysSecure

-- A Storer that expects to be provided with a file containing
-- the content of the key to store.
fileStorer :: (Key -> FilePath -> MeterUpdate -> Annex ()) -> Storer
fileStorer a k (FileContent f) m = a k f m
fileStorer a k (ByteContent b) m = withTmp k $ \f -> do
	let f' = fromRawFilePath f
	liftIO $ L.writeFile f' b
	a k f' m

-- A Storer that expects to be provided with a L.ByteString of
-- the content to store.
byteStorer :: (Key -> L.ByteString -> MeterUpdate -> Annex ()) -> Storer
byteStorer a k c m = withBytes c $ \b -> a k b m

-- A Retriever that writes the content of a Key to a provided file.
-- It is responsible for updating the progress meter as it retrieves data.
fileRetriever :: (FilePath -> Key -> MeterUpdate -> Annex ()) -> Retriever
fileRetriever a k m callback = do
	f <- prepTmp k
	a (fromRawFilePath f) k m
	pruneTmpWorkDirBefore f (callback . FileContent . fromRawFilePath)

-- A Retriever that generates a lazy ByteString containing the Key's
-- content, and passes it to a callback action which will fully consume it
-- before returning.
byteRetriever :: (Key -> (L.ByteString -> Annex ()) -> Annex ()) -> Retriever
byteRetriever a k _m callback = a k (callback . ByteContent)

{- The base Remote that is provided to specialRemote needs to have
 - storeKey, retrieveKeyFile, removeKey, and checkPresent methods,
 - but they are never actually used (since specialRemote replaces them).
 - Here are some dummy ones.
 -}
storeKeyDummy :: Key -> AssociatedFile -> MeterUpdate -> Annex ()
storeKeyDummy _ _ _ = error "missing storeKey implementation"
retrieveKeyFileDummy :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Verification
retrieveKeyFileDummy _ _ _ _ = error "missing retrieveKeyFile implementation"
removeKeyDummy :: Key -> Annex ()
removeKeyDummy _ = error "missing removeKey implementation"
checkPresentDummy :: Key -> Annex Bool
checkPresentDummy _ = error "missing checkPresent implementation"

type RemoteModifier
	= ParsedRemoteConfig
	-> Storer
	-> Retriever
	-> Remover
	-> CheckPresent
	-> Remote
	-> Remote

data SpecialRemoteCfg = SpecialRemoteCfg
	{ chunkConfig :: ChunkConfig
	, displayProgress :: Bool
	}

specialRemoteCfg :: ParsedRemoteConfig -> SpecialRemoteCfg
specialRemoteCfg c = SpecialRemoteCfg (getChunkConfig c) True

-- Modifies a base RemoteType to support chunking and encryption configs.
specialRemoteType :: RemoteType -> RemoteType
specialRemoteType r = r 
	{ configParser = \c -> addRemoteConfigParser specialRemoteConfigParsers
		<$> configParser r c
	}

specialRemoteConfigParsers :: [RemoteConfigFieldParser]
specialRemoteConfigParsers = chunkConfigParsers ++ encryptionConfigParsers

-- Modifies a base Remote to support both chunking and encryption,
-- which special remotes typically should support.
-- 
-- Handles progress displays when displayProgress is set.
specialRemote :: RemoteModifier
specialRemote c = specialRemote' (specialRemoteCfg c) c

specialRemote' :: SpecialRemoteCfg -> RemoteModifier
specialRemote' cfg c storer retriever remover checkpresent baser = encr
  where
	encr = baser
		{ storeKey = \k _f p -> cip >>= storeKeyGen k p
		, retrieveKeyFile = \k _f d p -> cip >>= retrieveKeyFileGen k d p
		, retrieveKeyFileCheap = case retrieveKeyFileCheap baser of
			Nothing -> Nothing
			Just a
				-- retrieval of encrypted keys is never cheap
				| isencrypted -> Nothing
				| otherwise -> Just $ \k f d -> a k f d
		-- When encryption is used, the remote could provide
		-- some other content encrypted by the user, and trick
		-- git-annex into decrypting it, leaking the decryption
		-- into the git-annex repository. Verifiable keys
		-- are the main protection against this attack.
		, retrievalSecurityPolicy = if isencrypted
			then mkRetrievalVerifiableKeysSecure (gitconfig baser)
			else retrievalSecurityPolicy baser
		, removeKey = \k -> cip >>= removeKeyGen k
		, checkPresent = \k -> cip >>= checkPresentGen k
		, cost = if isencrypted
			then cost baser + encryptedRemoteCostAdj
			else cost baser
		, getInfo = do
			l <- getInfo baser
			return $ l ++
				[ ("encryption", describeEncryption c)
				, ("chunking", describeChunkConfig (chunkConfig cfg))
				]
		, whereisKey = if noChunks (chunkConfig cfg) && not isencrypted
			then whereisKey baser
			else Nothing
		, exportActions = (exportActions baser)
			{ storeExport = \f k l p -> displayprogress p k (Just f) $
				storeExport (exportActions baser) f k l
			, retrieveExport = \k l f p -> displayprogress p k Nothing $
				retrieveExport (exportActions baser) k l f
			}
		}
	cip = cipherKey c (gitconfig baser)
	isencrypted = isEncrypted c

	-- chunk, then encrypt, then feed to the storer
	storeKeyGen k p enc = sendAnnex k rollback $ \src ->
		displayprogress p k (Just src) $ \p' ->
			storeChunks (uuid baser) chunkconfig enck k src p'
				enc encr storer checkpresent
	  where
		rollback = void $ removeKey encr k
		enck = maybe id snd enc

	-- call retriever to get chunks; decrypt them; stream to dest file
	retrieveKeyFileGen k dest p enc = do
		displayprogress p k Nothing $ \p' ->
			retrieveChunks retriever (uuid baser) chunkconfig
				enck k dest p' enc encr
		return UnVerified
	  where
		enck = maybe id snd enc

	removeKeyGen k enc = 
		removeChunks remover (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc

	checkPresentGen k enc = 
		checkPresentChunks checkpresent (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc

	chunkconfig = chunkConfig cfg

	displayprogress p k srcfile a
		| displayProgress cfg =
			metered (Just p) (KeySizer k (pure (fmap toRawFilePath srcfile))) (const a)
		| otherwise = a p

withBytes :: ContentSource -> (L.ByteString -> Annex a) -> Annex a
withBytes (ByteContent b) a = a b
withBytes (FileContent f) a = a =<< liftIO (L.readFile f)
