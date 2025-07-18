{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Remote.Directory (
	remote,
	finalizeStoreGeneric,
	removeDirGeneric,
) where

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.Default
import System.PosixCompat.Files (isRegularFile, deviceID)
#ifndef mingw32_HOST_OS
import System.PosixCompat.Files (getFdStatus)
#endif

import Annex.Common
import Types.Remote
import Types.Export
import Types.Creds
import qualified Git
import Config.Cost
import Config
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.Path
import Types.Import
import qualified Remote.Directory.LegacyChunked as Legacy
import Annex.CopyFile
import Annex.Content
import Annex.Perms
import Annex.UUID
import Annex.Verify
import Backend
import Types.KeySource
import Types.ProposedAccepted
import Utility.Metered
import Utility.Tmp
import Utility.InodeCache
import Utility.FileMode
import Utility.Directory.Create
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F
#ifndef mingw32_HOST_OS
import Utility.OpenFd
#endif

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "directory"
	, enumerate = const (findSpecialRemotes "directory")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser directoryField
			(FieldDesc "(required) where the special remote stores data")
		, yesNoParser ignoreinodesField (Just False)
			(FieldDesc "ignore inodes when importing/exporting")
		]
	, setup = directorySetup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = False
	}

directoryField :: RemoteConfigField
directoryField = Accepted "directory"

ignoreinodesField :: RemoteConfigField
ignoreinodesField = Accepted "ignoreinodes"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c cheapRemoteCost
	let chunkconfig = getChunkConfig c
	cow <- liftIO newCopyCoWTried
	fastcopy <- getFastCopy gc
	let ii = IgnoreInodes $ fromMaybe True $
		getRemoteConfigValue ignoreinodesField c
	return $ Just $ specialRemote c
		(storeKeyM dir chunkconfig cow fastcopy)
		(retrieveKeyFileM dir chunkconfig cow fastcopy)
		(removeKeyM dir)
		(checkPresentM dir chunkconfig)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileInOrder = pure True
			, retrieveKeyFileCheap = retrieveKeyFileCheapM dir chunkconfig
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = True
			, exportActions = ExportActions
				{ storeExport = storeExportM dir cow fastcopy
				, retrieveExport = retrieveExportM dir cow fastcopy
				, removeExport = removeExportM dir
				, checkPresentExport = checkPresentExportM dir
				-- Not needed because removeExportLocation
				-- auto-removes empty directories.
				, removeExportDirectory = Nothing
				, renameExport = Just $ renameExportM dir
				}
			, importActions = ImportActions
				{ listImportableContents = listImportableContentsM ii dir
				, importKey = Just (importKeyM ii dir)
				, retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierM ii dir cow
				, storeExportWithContentIdentifier = storeExportWithContentIdentifierM ii dir cow fastcopy
				, removeExportWithContentIdentifier = removeExportWithContentIdentifierM ii dir
				-- Not needed because removeExportWithContentIdentifier
				-- auto-removes empty directories.
				, removeExportDirectoryWhenEmpty = Nothing
				, checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierM ii dir
				}
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Just dir
			, readonly = False
			, appendonly = False
			, untrustworthy = False
			, availability = checkPathAvailability True dir
			, remotetype = remote
			, mkUnavailable = gen r u rc
				(gc { remoteAnnexDirectory = Just "/dev/null" }) rs
			, getInfo = return [("directory", dir')]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
  where
	dir = toOsPath dir'
	dir' = fromMaybe (giveup "missing directory")
		(remoteAnnexDirectory gc)

directorySetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
directorySetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let dir = maybe (giveup "Specify directory=") fromProposedAccepted $
		M.lookup directoryField c
	absdir <- liftIO $ absPath (toOsPath dir)
	liftIO $ unlessM (doesDirectoryExist absdir) $
		giveup $ "Directory does not exist: " ++ fromOsPath absdir
	(c', _encsetup) <- encryptionSetup c gc

	-- The directory is stored in git config, not in this remote's
	-- persistent state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("directory", fromOsPath absdir)]
	return (M.delete directoryField c', u)

{- Locations to try to access a given Key in the directory.
 - We try more than one since we used to write to different hash
 - directories. -}
locations :: OsPath -> Key -> NE.NonEmpty OsPath
locations d k = NE.map (d </>) (keyPaths k)

locations' :: OsPath -> Key -> [OsPath]
locations' d k = NE.toList (locations d k)

{- Returns the location of a Key in the directory. If the key is
 - present, returns the location that is actually used, otherwise
 - returns the first, default location. -}
getLocation :: OsPath -> Key -> IO OsPath
getLocation d k = do
	let locs = locations d k
	fromMaybe (NE.head locs) <$> firstM doesFileExist (NE.toList locs)

{- Directory where the file(s) for a key are stored. -}
storeDir :: OsPath -> Key -> OsPath
storeDir d k = addTrailingPathSeparator $
	d </> hashDirLower def k </> keyFile k

{- Check if there is enough free disk space in the remote's directory to
 - store the key. Note that the unencrypted key size is checked. -}
storeKeyM :: OsPath -> ChunkConfig -> CopyCoWTried -> FastCopy -> Storer
storeKeyM d chunkconfig cow fastcopy k c m = 
	ifM (checkDiskSpaceDirectory d k)
		( do
			void $ liftIO $ tryIO $ createDirectoryUnder [d] tmpdir
			store
		, giveup "Not enough free disk space."
		)
  where
	store = case chunkconfig of
		LegacyChunks chunksize -> 
			let go _k b p = liftIO $ Legacy.store
				(fromOsPath d)
				chunksize
				(finalizeStoreGeneric d)
				k b p
				(fromOsPath tmpdir)
				(fromOsPath destdir)
			in byteStorer go k c m
		NoChunks ->
			let go _k src p = liftIO $ do
				void $ fileCopier cow fastcopy src tmpf p Nothing
				finalizeStoreGeneric d tmpdir destdir
			in fileStorer go k c m
		_ -> 
			let go _k b p = liftIO $ do
				meteredWriteFile p tmpf b
				finalizeStoreGeneric d tmpdir destdir
			in byteStorer go k c m
	
	tmpdir = addTrailingPathSeparator $ d </> literalOsPath "tmp" </> kf
	tmpf = tmpdir </> kf
	kf = keyFile k
	destdir = storeDir d k

checkDiskSpaceDirectory :: OsPath -> Key -> Annex Bool
checkDiskSpaceDirectory d k = do
	annexdir <- fromRepo gitAnnexObjectDir
	samefilesystem <- liftIO $ catchDefaultIO False $ 
		(\a b -> deviceID a == deviceID b)
			<$> R.getSymbolicLinkStatus (fromOsPath d)
			<*> R.getSymbolicLinkStatus (fromOsPath annexdir)
	checkDiskSpace Nothing (Just d) k 0 samefilesystem

{- Passed a temp directory that contains the files that should be placed
 - in the dest directory, moves it into place. Anything already existing
 - in the dest directory will be deleted. File permissions will be locked
 - down. -}
finalizeStoreGeneric :: OsPath -> OsPath -> OsPath -> IO ()
finalizeStoreGeneric d tmp dest = do
	removeDirGeneric False d dest
	createDirectoryUnder [d] (parentDir dest)
	renameDirectory tmp dest
	-- may fail on some filesystems
	void $ tryIO $ do
		mapM_ preventWrite =<< dirContents dest
		preventWrite dest

retrieveKeyFileM :: OsPath -> ChunkConfig -> CopyCoWTried -> FastCopy -> Retriever
retrieveKeyFileM d (LegacyChunks _) _ _ = Legacy.retrieve locations' d
retrieveKeyFileM d NoChunks cow fastcopy = fileRetriever' $ \dest k p iv -> do
	src <- liftIO $ getLocation d k
	void $ liftIO $ fileCopier cow fastcopy src dest p iv
retrieveKeyFileM d _ _ _ = byteRetriever $ \k sink ->
	sink =<< liftIO (F.readFile =<< getLocation d k)

retrieveKeyFileCheapM :: OsPath -> ChunkConfig -> Maybe (Key -> AssociatedFile -> OsPath -> Annex ())
-- no cheap retrieval possible for chunks
retrieveKeyFileCheapM _ (UnpaddedChunks _) = Nothing
retrieveKeyFileCheapM _ (LegacyChunks _) = Nothing
#ifndef mingw32_HOST_OS
retrieveKeyFileCheapM d NoChunks = Just $ \k _af f -> liftIO $ do
	file <- absPath =<< getLocation d k
	ifM (doesFileExist file)
		( R.createSymbolicLink (fromOsPath file) (fromOsPath f)
		, giveup "content file not present in remote"
		)
#else
retrieveKeyFileCheapM _ _ = Nothing
#endif

removeKeyM :: OsPath -> Remover
removeKeyM d _proof k = liftIO $ removeDirGeneric True d (storeDir d k)

{- Removes the directory, which must be located under the topdir.
 -
 - Succeeds even on directories and contents that do not have write
 - permission, if it's possible to turn the write bit on.
 -
 - If the directory does not exist, succeeds as long as the topdir does
 - exist. If the topdir does not exist, fails, because in this case the
 - remote is not currently accessible and probably still has the content
 - we were supposed to remove from it.
 -
 - Empty parent directories (up to but not including the topdir)
 - can also be removed. Failure to remove such a directory is not treated
 - as an error.
 -}
removeDirGeneric :: Bool -> OsPath -> OsPath -> IO ()
removeDirGeneric removeemptyparents topdir dir = do
	void $ tryIO $ allowWrite dir
#ifdef mingw32_HOST_OS
	{- Windows needs the files inside the directory to be writable
	 - before it can delete them. -}
	void $ tryIO $ mapM_ allowWrite =<< dirContents dir
#endif
	tryNonAsync (removeDirectoryRecursive dir) >>= \case
		Right () -> return ()
		Left e ->
			unlessM (doesDirectoryExist topdir <&&> (not <$> doesDirectoryExist dir)) $
				throwM e
	when removeemptyparents $ do
		subdir <- relPathDirToFile topdir (takeDirectory dir)
		goparents (Just (takeDirectory subdir)) (Right ())
  where
	goparents _ (Left _e) = return ()
	goparents Nothing _ = return ()
	goparents (Just subdir) _ = do
		let d = topdir </> subdir
		goparents (upFrom subdir) =<< tryIO (removeDirectory d)

checkPresentM :: OsPath -> ChunkConfig -> CheckPresent
checkPresentM d (LegacyChunks _) k = Legacy.checkKey d locations' k
checkPresentM d _ k = checkPresentGeneric d (locations' d k)

checkPresentGeneric :: OsPath -> [OsPath] -> Annex Bool
checkPresentGeneric d ps = checkPresentGeneric' d $
	liftIO $ anyM doesFileExist ps

checkPresentGeneric' :: OsPath -> Annex Bool -> Annex Bool
checkPresentGeneric' d check = ifM check
	( return True
	, ifM (liftIO $ doesDirectoryExist d)
		( return False
		, giveup $ "directory " ++ fromOsPath d ++ " is not accessible"
		)
	)

storeExportM :: OsPath -> CopyCoWTried -> FastCopy -> OsPath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportM d cow fastcopy src _k loc p = do
	liftIO $ createDirectoryUnder [d] (takeDirectory dest)
	-- Write via temp file so that checkPresentGeneric will not
	-- see it until it's fully stored.
	viaTmp go dest ()
  where
	dest = exportPath d loc
	go tmp () = void $ liftIO $
		fileCopier cow fastcopy src tmp p Nothing

retrieveExportM :: OsPath -> CopyCoWTried -> FastCopy -> Key -> ExportLocation -> OsPath -> MeterUpdate -> Annex Verification
retrieveExportM d cow fastcopy k loc dest p = 
	verifyKeyContentIncrementally AlwaysVerify k $ \iv -> 
		void $ liftIO $ fileCopier cow fastcopy src dest p iv
  where
	src = exportPath d loc

removeExportM :: OsPath -> Key -> ExportLocation -> Annex ()
removeExportM d _k loc = liftIO $ do
	removeWhenExistsWith removeFile src
	removeExportLocation d loc
  where
	src = exportPath d loc

checkPresentExportM :: OsPath -> Key -> ExportLocation -> Annex Bool
checkPresentExportM d _k loc =
	checkPresentGeneric d [exportPath d loc]

renameExportM :: OsPath -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportM d _k oldloc newloc = liftIO $ do
	createDirectoryUnder [d] (takeDirectory dest)
	renameFile src dest
	removeExportLocation d oldloc
	return (Just ())
  where
	src = exportPath d oldloc
	dest = exportPath d newloc

exportPath :: OsPath -> ExportLocation -> OsPath
exportPath d loc = d </> fromExportLocation loc

{- Removes the ExportLocation's parent directory and its parents, so long as
 - they're empty, up to but not including the topdir. -}
removeExportLocation :: OsPath -> ExportLocation -> IO ()
removeExportLocation topdir loc = 
	go (Just $ takeDirectory $ fromExportLocation loc) (Right ())
  where
	go _ (Left _e) = return ()
	go Nothing _ = return ()
	go (Just loc') _ = 
		let p = exportPath topdir $ mkExportLocation loc'
		in go (upFrom loc') =<< tryIO (removeDirectory p)

listImportableContentsM :: IgnoreInodes -> OsPath -> Annex (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)))
listImportableContentsM ii dir = liftIO $ do
	l' <- mapM go =<< dirContentsRecursiveSkipping (const False) False dir
	return $ Just $ ImportableContentsComplete $
		ImportableContents (catMaybes l') []
  where
	go f = do
		st <- R.getSymbolicLinkStatus (fromOsPath f)
		mkContentIdentifier ii f st >>= \case
			Nothing -> return Nothing
			Just cid -> do
				relf <- relPathDirToFile dir f
				sz <- getFileSize' f st
				return $ Just (mkImportLocation relf, (cid, sz))

newtype IgnoreInodes = IgnoreInodes Bool

-- Make a ContentIdentifier that contains the size and mtime of the file,
-- and also normally the inode, unless ignoreinodes=yes.
--
-- If the file is not a regular file, this will return Nothing.
mkContentIdentifier :: IgnoreInodes -> OsPath -> FileStatus -> IO (Maybe ContentIdentifier)
mkContentIdentifier (IgnoreInodes ii) f st =
	liftIO $ fmap (ContentIdentifier . encodeBS . showInodeCache)
		<$> if ii
			then toInodeCache' noTSDelta f st 0
			else toInodeCache noTSDelta f st

-- Since ignoreinodes can be changed by enableremote, and since previous
-- versions of git-annex ignored inodes by default, treat two content
-- identifiers as the same if they differ only by one having the inode
-- ignored.
guardSameContentIdentifiers :: a -> [ContentIdentifier] -> Maybe ContentIdentifier -> a
guardSameContentIdentifiers _ _ Nothing = giveup "file not found"
guardSameContentIdentifiers cont olds (Just new)
	| any (new ==) olds = cont
	| any (ignoreinode new ==) olds = cont
	| any (\old -> new == ignoreinode old) olds = cont
	| otherwise = giveup "file content has changed"
  where
	ignoreinode cid@(ContentIdentifier b) = 
		case readInodeCache (decodeBS b) of
			Nothing -> cid
			Just ic -> 
				let ic' = replaceInode 0 ic
				in ContentIdentifier (encodeBS (showInodeCache ic'))

importKeyM :: IgnoreInodes -> OsPath -> ExportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> Annex (Maybe Key)
importKeyM ii dir loc cid sz p = do
	backend <- chooseBackend f
	unsizedk <- fst <$> genKey ks p backend
	let k = alterKey unsizedk $ \kd -> kd
		{ keySize = keySize kd <|> Just sz }
	currcid <- liftIO $ mkContentIdentifier ii absf
		=<< R.getSymbolicLinkStatus (fromOsPath absf)
	guardSameContentIdentifiers (return (Just k)) [cid] currcid
  where
	f = fromExportLocation loc
	absf = dir </> f
	ks  = KeySource
		{ keyFilename = f
		, contentLocation = absf
		, inodeCache = Nothing
		}

retrieveExportWithContentIdentifierM :: IgnoreInodes -> OsPath -> CopyCoWTried -> ExportLocation -> [ContentIdentifier] -> OsPath -> Either Key (Annex Key) -> MeterUpdate -> Annex (Key, Verification)
retrieveExportWithContentIdentifierM ii dir cow loc cids dest gk p =
	case gk of
		Right mkkey -> do
			go Nothing
			k <- mkkey
			return (k, UnVerified)
		Left k -> do
			v <- verifyKeyContentIncrementally DefaultVerify k go
			return (k, v)
  where
	f = exportPath dir loc
	f' = fromOsPath f

	go iv = precheck (docopy iv)

	docopy iv = ifM (liftIO $ tryCopyCoW cow f dest p)
		( postcheckcow (liftIO $ maybe noop unableIncrementalVerifier iv)
		, docopynoncow iv
		)

	docopynoncow iv = do
#ifndef mingw32_HOST_OS
		let open = do
			-- Need a duplicate fd for the post check.
			fd <- openFdWithMode f' ReadOnly Nothing defaultFileFlags
			dupfd <- dup fd
			h <- fdToHandle fd
			return (h, dupfd)
		let close (h, dupfd) = do
			hClose h
			closeFd dupfd
		bracketIO open close $ \(h, dupfd) -> do
#else
		let open = F.openBinaryFile f ReadMode
		let close = hClose
		bracketIO open close $ \h -> do
#endif
			liftIO $ fileContentCopier h dest p iv
#ifndef mingw32_HOST_OS
			postchecknoncow dupfd (return ())
#else
			postchecknoncow (return ())
#endif
	
	-- Check before copy, to avoid expensive copy of wrong file
	-- content.
	precheck cont = guardSameContentIdentifiers cont cids
		=<< liftIO . mkContentIdentifier ii f
		=<< liftIO (R.getSymbolicLinkStatus f')

	-- Check after copy, in case the file was changed while it was
	-- being copied.
	--
	-- When possible (not on Windows), check the same handle
	-- that the file was copied from. Avoids some race cases where
	-- the file is modified while it's copied but then gets restored
	-- to the original content afterwards.
	--
	-- This does not guard against every possible race, but neither
	-- can InodeCaches detect every possible modification to a file.
	-- It's probably as good or better than git's handling of similar
	-- situations with files being modified while it's updating the
	-- working tree for a merge.
#ifndef mingw32_HOST_OS
	postchecknoncow fd cont = do
#else
	postchecknoncow cont = do
#endif
		currcid <- liftIO $ mkContentIdentifier ii f
#ifndef mingw32_HOST_OS
			=<< getFdStatus fd
#else
			=<< R.getSymbolicLinkStatus f'
#endif
		guardSameContentIdentifiers cont cids currcid

	-- When copy-on-write was done, cannot check the handle that was
	-- copied from, but such a copy should run very fast, so
	-- it's very unlikely that the file changed after precheck,
	-- the modified version was copied CoW, and then the file was
	-- restored to the original content before this check.
	postcheckcow cont = do
		currcid <- liftIO $ mkContentIdentifier ii f
			=<< R.getSymbolicLinkStatus f'
		guardSameContentIdentifiers cont cids currcid

storeExportWithContentIdentifierM :: IgnoreInodes -> OsPath -> CopyCoWTried -> FastCopy -> OsPath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
storeExportWithContentIdentifierM ii dir cow fastcopy src _k loc overwritablecids p = do
	liftIO $ createDirectoryUnder [dir] destdir
	withTmpFileIn destdir template $ \tmpf tmph -> do
		let tmpf' = fromOsPath tmpf
		liftIO $ hClose tmph
		void $ liftIO $ fileCopier cow fastcopy src tmpf p Nothing
		resetAnnexFilePerm tmpf
		liftIO (R.getSymbolicLinkStatus tmpf') >>= liftIO . mkContentIdentifier ii tmpf >>= \case
			Nothing -> giveup "unable to generate content identifier"
			Just newcid -> do
				checkExportContent ii dir loc
					overwritablecids
					(giveup "unsafe to overwrite file")
					(const $ liftIO $ R.rename tmpf' (fromOsPath dest))
				return newcid
  where
	dest = exportPath dir loc
	(destdir, base) = splitFileName dest
	template = relatedTemplate (fromOsPath base <> ".tmp")

removeExportWithContentIdentifierM :: IgnoreInodes -> OsPath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
removeExportWithContentIdentifierM ii dir k loc removeablecids =
	checkExportContent ii dir loc removeablecids (giveup "unsafe to remove modified file") $ \case
		DoesNotExist -> return ()
		KnownContentIdentifier -> removeExportM dir k loc

checkPresentExportWithContentIdentifierM :: IgnoreInodes -> OsPath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM ii dir _k loc knowncids =
	checkPresentGeneric' dir $
		checkExportContent ii dir loc knowncids (return False) $ \case
			DoesNotExist -> return False
			KnownContentIdentifier -> return True

data CheckResult = DoesNotExist | KnownContentIdentifier

-- Checks if the content at an ExportLocation is in the knowncids,
-- and only runs the callback that modifies it if it's safe to do so.
--
-- This should avoid races to the extent possible. However,
-- if something has the file open for write, it could write to the handle
-- after the callback has overwritten or deleted it, and its write would
-- be lost, and we don't need to detect that.
-- (In similar situations, git doesn't either!) 
--
-- It follows that if something is written to the destination file
-- shortly before, it's acceptable to run the callback anyway, as that's
-- nearly indistinguishable from the above case.
--
-- So, it suffices to check if the destination file's current
-- content is known, and immediately run the callback.
checkExportContent :: IgnoreInodes -> OsPath -> ExportLocation -> [ContentIdentifier] -> Annex a -> (CheckResult -> Annex a) -> Annex a
checkExportContent ii dir loc knowncids unsafe callback = 
	tryWhenExists (liftIO $ R.getSymbolicLinkStatus (fromOsPath dest)) >>= \case
		Just destst
			| not (isRegularFile destst) -> unsafe
			| otherwise -> catchDefaultIO Nothing (liftIO $ mkContentIdentifier ii dest destst) >>= \case
				Just destcid
					| destcid `elem` knowncids -> callback KnownContentIdentifier
					-- dest exists with other content
					| otherwise -> unsafe
				-- should never happen
				Nothing -> unsafe
		-- dest does not exist
		Nothing -> callback DoesNotExist
  where
	dest = exportPath dir loc
