{- git-annex command
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Fsck where

import Command
import qualified Annex
import qualified Remote
import qualified Types.Backend
import qualified Backend
import qualified Git
import Annex.Content
import Annex.Verify
#ifndef mingw32_HOST_OS
import Annex.Version
import Annex.Content.Presence
#endif
import Annex.Content.Presence.LowLevel
import Annex.Perms
import Annex.Link
import Annex.Fixup
import Logs.Location
import Logs.Trust
import Logs.Activity
import Utility.TimeStamp
import Logs.PreferredContent
import Annex.NumCopies
import Annex.UUID
import Annex.ReplaceFile
import Utility.DataUnits
import Utility.HumanTime
import Utility.CopyFile
import Git.FilePath
import Utility.PID
import Utility.InodeCache
import Utility.Metered
import Annex.InodeSentinal
import qualified Database.Keys
import qualified Database.Fsck as FsckDb
import Types.CleanupActions
import Types.Key
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F

import Data.Time.Clock.POSIX
import System.Posix.Types (EpochTime)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either
import System.PosixCompat.Files (fileMode, isSymbolicLink, modificationTime)

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, annexedMatchingOptions] $
	command "fsck" SectionMaintenance
		"find and fix problems"
		paramPaths (seek <$$> optParser)

data FsckOptions = FsckOptions
	{ fsckFiles :: CmdParams
	, fsckFromOption :: Maybe (DeferredParse Remote)
	, incrementalOpt :: Maybe IncrementalOpt
	, keyOptions :: Maybe KeyOptions
	}

data IncrementalOpt
	= StartIncrementalO
	| MoreIncrementalO
	| ScheduleIncrementalO Duration

optParser :: CmdParamsDesc -> Parser FsckOptions
optParser desc = FsckOptions
	<$> cmdParams desc
	<*> optional (mkParseRemoteOption <$> strOption 
		( long "from" <> short 'f' <> metavar paramRemote 
		<> help "check remote"
		<> completeRemotes
		))
	<*> optional parseincremental
	<*> optional parseKeyOptions
  where
	parseincremental =
		flag' StartIncrementalO
			( long "incremental" <> short 'S'
			<> help "start an incremental fsck"
			)
		<|> flag' MoreIncrementalO
			( long "more" <> short 'm'
			<> help "continue an incremental fsck"
			)
		<|> (ScheduleIncrementalO <$> option (eitherReader parseDuration)
			( long "incremental-schedule" <> metavar paramTime
			<> help "schedule incremental fscking"
			))

seek :: FsckOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (fsckFromOption o)
	u <- maybe getUUID (pure . Remote.uuid) from
	checkDeadRepo u
	when (isNothing from) $
		cleanupLinkedWorkTreeBug
	i <- prepIncremental u (incrementalOpt o)
	let seeker = AnnexedFileSeeker
		{ startAction = const $ start from i
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}
	withKeyOptions (keyOptions o) False seeker
		(\kai -> commandAction . startKey from i kai =<< getNumCopies)
		(withFilesInGitAnnex ww seeker)
		=<< workTreeItems ww (fsckFiles o)
	cleanupIncremental i
	void $ tryIO $ recordActivity Fsck u
  where
	ww = WarnUnmatchLsFiles "fsck"

checkDeadRepo :: UUID -> Annex ()
checkDeadRepo u =
	whenM ((==) DeadTrusted <$> lookupTrust u) $
		earlyWarning "Warning: Fscking a repository that is currently marked as dead."

start :: Maybe Remote -> Incremental -> SeekInput -> OsPath -> Key -> CommandStart
start from inc si file key = Backend.getBackend file key >>= \case
	Nothing -> stop
	Just backend -> do
		(numcopies, _mincopies) <- getFileNumMinCopies file
		case from of
			Nothing -> go $ perform key file backend numcopies
			Just r -> go $ performRemote key afile numcopies r
  where
	go = runFsck inc si (mkActionItem (key, afile)) key
	afile = AssociatedFile (Just file)

perform :: Key -> OsPath -> Backend -> NumCopies -> Annex Bool
perform key file backend numcopies = do
	keystatus <- getKeyFileStatus key file
	check
		-- order matters
		[ fixLink key file
		, fixObjectLocation key
		, verifyLocationLog key keystatus ai
		, verifyRequiredContent key ai
		, verifyAssociatedFiles key keystatus file
		, verifyWorkTree key file
		, checkKeySize key keystatus ai
		, checkBackend key keystatus afile
		, checkKeyUpgrade backend key ai afile
		, checkKeyNumCopies key afile numcopies
		]
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (key, afile)

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> AssociatedFile -> NumCopies -> Remote -> Annex Bool
performRemote key afile numcopies remote =
	dispatch =<< Remote.hasKey remote key
  where
	dispatch (Left err) = do
		showNote (UnquotedString err)
		return False
	dispatch (Right True) = withtmp $ \tmpfile ->
		getfile tmpfile >>= \case
			Nothing -> go True Nothing
			Just (Right verification) -> go True (Just (tmpfile, verification))
			Just (Left _) -> do
				warning $ actionItemDesc ai
					<> ": failed to download file from remote"
				void $ go True Nothing
				return False
	dispatch (Right False) = go False Nothing
	go present lv = check
		[ verifyLocationLogRemote key ai remote present
		, verifyRequiredContent key ai
		, withLocalCopy (fmap fst lv) $ checkKeySizeRemote key remote ai
		, case fmap snd lv of
			Just Verified -> return True
			_ -> withLocalCopy (fmap fst lv) $
				checkBackendRemote key remote ai
		, checkKeyNumCopies key afile numcopies
		]
	ai = mkActionItem (key, afile)
	withtmp a = do
		-- Put it in the gitAnnexTmpObjectDir since that's on a
		-- filesystem where object temp files are normally
		-- stored. The pid prevents multiple fsck processes
		-- contending over the same file. (Multiple threads cannot,
		-- because OnlyActionOn is used.)
		pid <- liftIO getPID
		t <- fromRepo gitAnnexTmpObjectDir
		createAnnexDirectory t
		let tmp = t </> literalOsPath "fsck" <> toOsPath (show pid) <> literalOsPath "." <> keyFile key
		let cleanup = liftIO $ catchIO (removeFile tmp) (const noop)
		cleanup
		cleanup `after` a tmp
	getfile tmp = ifM (checkDiskSpace Nothing (Just (takeDirectory tmp)) key 0 True)
		( ifM (getcheap tmp)
			( return (Just (Right UnVerified))
			, ifM (Annex.getRead Annex.fast)
				( return Nothing
				, Just <$> tryNonAsync (getfile' tmp)
				)
			)
		, return Nothing
		)
	getfile' tmp = Remote.retrieveKeyFile remote key (AssociatedFile Nothing) tmp nullMeterUpdate (RemoteVerify remote)
	getcheap tmp = case Remote.retrieveKeyFileCheap remote of
		Just a -> isRight <$> tryNonAsync (a key afile tmp)
		Nothing -> return False

startKey :: Maybe Remote -> Incremental -> (SeekInput, Key, ActionItem) -> NumCopies -> CommandStart
startKey from inc (si, key, ai) numcopies =
	Backend.maybeLookupBackendVariety (fromKey keyVariety key) >>= \case
		Nothing -> stop
		Just _ -> runFsck inc si ai key $
			case from of
				Nothing -> performKey key numcopies
				Just r -> performRemote key (AssociatedFile Nothing) numcopies r

performKey :: Key -> NumCopies -> Annex Bool
performKey key numcopies = do
	keystatus <- getKeyStatus key
	check
		[ verifyLocationLog key keystatus (mkActionItem key)
		, checkKeySize key keystatus (mkActionItem key)
		, checkBackend key keystatus (AssociatedFile Nothing)
		, checkKeyNumCopies key (AssociatedFile Nothing) numcopies
		]

check :: [Annex Bool] -> Annex Bool
check cs = and <$> sequence cs

{- Checks that symlinks points correctly to the annexed content. -}
fixLink :: Key -> OsPath -> Annex Bool
fixLink key file = do
	want <- calcRepo $ gitAnnexLink file key
	have <- fmap toOsPath <$> getAnnexLinkTarget file
	maybe noop (go want) have
	return True
  where
	go want have
		| want /= fromInternalGitPath have = do
			showNote "fixing link"
			createWorkTreeDirectory (parentDir file)
			liftIO $ removeFile file
			addAnnexLink (fromOsPath want) file
		| otherwise = noop

{- A repository that supports symlinks and is not bare may have in the past
 - been bare, or not supported symlinks. If so, the object may be located
 - in a directory other than the one where annex symlinks point to. Moves
 - the object in that case.
 -
 - Also if a repository has been converted to bare, or moved to a crippled
 - filesystem not supporting symlinks, the object file will be moved
 - to the other location.
 -}
fixObjectLocation :: Key -> Annex Bool
#ifdef mingw32_HOST_OS
fixObjectLocation _key = do
	-- Windows does not allow locked files to be renamed, but annex
	-- links are also not used on Windows.
	return True
#else
fixObjectLocation key = do
	loc <- calcRepo (gitAnnexLocation key)
	idealloc <- calcRepo (gitAnnexLocation' (const (pure True)) key)
	if loc == idealloc
		then return True
		else ifM (liftIO $ doesPathExist loc)
			( moveobjdir loc idealloc
				`catchNonAsync` \_e -> return True
			, return True
			)
  where
	moveobjdir src dest = do
		let srcdir = parentDir src
		let destdir = parentDir dest
		showNote "normalizing object location"
		-- When the content file is moved, it will
		-- appear to other processes as if it has been removed.
		-- That should never happen to a process that has used
		-- lockContentShared, so avoid it by locking the content
		-- for removal, although it's not really being removed.
		lockContentForRemoval key (return True) $ \_lck -> do
			-- Thaw the content directory to allow renaming it.
			thawContentDir src
			createAnnexDirectory (parentDir destdir)
			liftIO $ renameDirectory srcdir destdir
			-- Since the directory was moved, lockContentForRemoval
			-- will not be able to remove the lock file it
			-- made. So, remove the lock file here.
			mlockfile <- contentLockFile key =<< getVersion
			liftIO $ maybe noop (removeWhenExistsWith removeFile) mlockfile
			freezeContentDir dest
			cleanObjectDirs src
			return True
#endif

{- Checks that the location log reflects the current status of the key,
 - in this repository only. -}
verifyLocationLog :: Key -> KeyStatus -> ActionItem -> Annex Bool
verifyLocationLog key keystatus ai = do
	obj <- calcRepo (gitAnnexLocation key)
	present <- if isKeyUnlockedThin keystatus
		then liftIO (doesFileExist obj)
		else inAnnex key
	u <- getUUID
	
	{- Since we're checking that a key's object file is present, throw
	 - in a permission fixup here too. -}
	when present $ do
		void $ tryIO $ case keystatus of
			KeyUnlockedThin -> thawContent obj
			KeyLockedThin -> thawContent obj
			_ -> freezeContent obj
		checkContentWritePerm obj >>= \case
			Nothing -> warning $ "** Unable to set correct write mode for " <> QuotedPath obj <> " ; perhaps you don't own that file, or perhaps it has an xattr or ACL set"
			_ -> return ()
	whenM (liftIO $ doesDirectoryExist $ parentDir obj) $
		freezeContentDir obj

	{- Warn when annex.securehashesonly is set and content using an 
	 - insecure hash is present. This should only be able to happen
	 - if the repository already contained the content before the
	 - config was set, or of course if a hash was broken. -}
	whenM (pure present <&&> (not <$> Backend.isCryptographicallySecureKey key)) $
		whenM (annexSecureHashesOnly <$> Annex.getGitConfig) $
			warning $ "** Despite annex.securehashesonly being set, " <> QuotedPath obj <> " has content present in the annex using an insecure " <> UnquotedString (decodeBS (formatKeyVariety (fromKey keyVariety key))) <> " key"

	verifyLocationLog' key ai present u (logChange NoLiveUpdate key u)

verifyLocationLogRemote :: Key -> ActionItem -> Remote -> Bool -> Annex Bool
verifyLocationLogRemote key ai remote present =
	verifyLocationLog' key ai present (Remote.uuid remote)
		(Remote.logStatus NoLiveUpdate remote key)

verifyLocationLog' :: Key -> ActionItem -> Bool -> UUID -> (LogStatus -> Annex ()) -> Annex Bool
verifyLocationLog' key ai present u updatestatus = do
	uuids <- loggedLocations key
	case (present, u `elem` uuids) of
		(True, False) -> do
			fix InfoPresent
			-- There is no data loss, so do not fail.
			return True
		(False, True) -> do
			fix InfoMissing
			warning $
				"** Based on the location log, " <>
				actionItemDesc ai <>
				"\n** was expected to be present, " <>
				"but its content is missing."
			return False
		(False, False) -> do
			-- When the location log for the key is not present,
			-- create it, so that the key will be known.
			when (null uuids) $
				whenM (not <$> isKnownKey key) $
					updatestatus InfoMissing
			return True
		(True, True) -> return True
  where
	fix s = do
		showNote "fixing location log"
		updatestatus s

{- Verifies that all repos that are required to contain the content do,
 - checking against the location log. -}
verifyRequiredContent :: Key -> ActionItem -> Annex Bool
verifyRequiredContent key ai@(ActionItemAssociatedFile afile _) = case afile of
	-- Can't be checked if there's no associated file.
	AssociatedFile Nothing -> return True
	AssociatedFile (Just _) -> do
		requiredlocs <- filterM notdead =<< (M.keys <$> requiredContentMap)
		if null requiredlocs
			then return True
			else go (S.fromList requiredlocs)
  where
	notdead u = (/=) DeadTrusted <$> lookupTrust u

	go requiredlocs = do
		presentlocs <- S.fromList <$> loggedLocations key
		missinglocs <- filterM
			(\u -> isRequiredContent NoLiveUpdate (Just u) S.empty (Just key) afile False)
			(S.toList $ S.difference requiredlocs presentlocs)
		if null missinglocs
			then return True
			else do
				missingrequired <- Remote.prettyPrintUUIDs "missingrequired" missinglocs
				warning $
					"** Required content " <>
					actionItemDesc ai <>
					" is missing from these repositories:\n" <>
					UnquotedString missingrequired
				return False
verifyRequiredContent _ _ = return True

{- Verifies the associated file records. -}
verifyAssociatedFiles :: Key -> KeyStatus -> OsPath -> Annex Bool
verifyAssociatedFiles key keystatus file = do
	when (isKeyUnlockedThin keystatus) $ do
		f <- inRepo $ toTopFilePath file
		afs <- Database.Keys.getAssociatedFiles key
		unless (getTopFilePath f `elem` map getTopFilePath afs) $
			Database.Keys.addAssociatedFile key f
	return True

verifyWorkTree :: Key -> OsPath -> Annex Bool
verifyWorkTree key file = do
	{- Make sure that a pointer file is replaced with its content,
	 - when the content is available. -}
	mk <- liftIO $ isPointerFile file
	case mk of
		Just k | k == key -> whenM (inAnnex key) $ do
			showNote "fixing worktree content"
			replaceWorkTreeFile file $ \tmp -> do
				mode <- liftIO $ catchMaybeIO $
					fileMode <$> R.getFileStatus
						(fromOsPath file)
				ifM (annexThin <$> Annex.getGitConfig)
					( void $ linkFromAnnex' key tmp mode
					, do
						obj <- calcRepo (gitAnnexLocation key)
						void $ checkedCopyFile key obj tmp mode
						thawContent tmp
					)
				Database.Keys.storeInodeCaches key [tmp]
		_ -> return ()
	return True

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available.
 -
 - Not checked when a file is unlocked.
 -}
checkKeySize :: Key -> KeyStatus -> ActionItem -> Annex Bool
checkKeySize _ KeyUnlockedThin _ = return True
checkKeySize key _ ai = do
	file <- calcRepo $ gitAnnexLocation key
	ifM (liftIO $ doesPathExist file)
		( checkKeySizeOr badContent key file ai
		, return True
		)

withLocalCopy :: Maybe OsPath -> (OsPath -> Annex Bool) -> Annex Bool
withLocalCopy Nothing _ = return True
withLocalCopy (Just localcopy) f = f localcopy

checkKeySizeRemote :: Key -> Remote -> ActionItem -> OsPath -> Annex Bool
checkKeySizeRemote key remote ai localcopy =
	checkKeySizeOr (badContentRemote remote localcopy) key localcopy ai

checkKeySizeOr :: (Key -> Annex String) -> Key -> OsPath -> ActionItem -> Annex Bool
checkKeySizeOr bad key file ai = case fromKey keySize key of
	Nothing -> return True
	Just size -> do
		size' <- liftIO $ getFileSize file
		comparesizes size size'
  where
	comparesizes a b = do
		let same = a == b
		unless same $ badsize a b
		return same
	badsize a b = do
		msg <- bad key
		warning $ actionItemDesc ai
			<> ": Bad file size ("
			<> UnquotedString (compareSizes storageUnits True a b)
			<> "); "
			<> UnquotedString msg

{- Check for keys that are upgradable.
 -
 - Warns and suggests the user migrate, but does not migrate itself,
 - because migration can cause more disk space to be used, and makes
 - worktree changes that need to be committed.
 -}
checkKeyUpgrade :: Backend -> Key -> ActionItem -> AssociatedFile -> Annex Bool
checkKeyUpgrade backend key ai (AssociatedFile (Just file)) =
	case Types.Backend.canUpgradeKey backend of
		Just a | a key -> do
			warning $ actionItemDesc ai
				<> ": Can be upgraded to an improved key format. "
				<> "You can do so by running: git annex migrate --backend="
				<> UnquotedByteString (formatKeyVariety (fromKey keyVariety key))
				<> " "
				<> QuotedPath file
			return True
		_ -> return True
checkKeyUpgrade _ _ _ (AssociatedFile Nothing) =
	-- Don't suggest migrating without a filename, because
	-- while possible to do, there is no actual benefit from
	-- doing that in this situation.
	return True

{- Runs the backend specific check on a key's content object.
 -
 - When a annex.thin is set, an unlocked file may be a hard link to the object.
 - Thus when the user modifies the file, the object will be modified and
 - not pass the check, and we don't want to find an error in this case.
 -}
checkBackend :: Key -> KeyStatus -> AssociatedFile -> Annex Bool
checkBackend key keystatus afile = do
	content <- calcRepo (gitAnnexLocation key)
	ifM (liftIO $ doesPathExist content)
		( ifM (pure (isKeyUnlockedThin keystatus) <&&> (not <$> isUnmodified key content))
			( nocheck
			, do
				mic <- withTSDelta (liftIO . genInodeCache content)
				ifM (checkBackendOr badContent key content ai)
					( do
						checkInodeCache key content mic ai
						return True
					, return False
					)
			)
		, nocheck
		)
  where
	nocheck = return True

	ai = mkActionItem (key, afile)

checkBackendRemote :: Key -> Remote -> ActionItem -> OsPath -> Annex Bool
checkBackendRemote key remote ai localcopy =
	checkBackendOr (badContentRemote remote localcopy) key localcopy ai

checkBackendOr :: (Key -> Annex String) -> Key -> OsPath -> ActionItem -> Annex Bool
checkBackendOr bad key file ai =
	ifM (Annex.getRead Annex.fast)
		( return True
		, do
			ok <- verifyKeyContent' key file
			unless ok $ do
				msg <- bad key
				warning $ actionItemDesc ai
					<> ": Bad file content; "
					<> UnquotedString msg
			return ok
		)

{- Check, if there are InodeCaches recorded for a key, that one of them
 - matches the object file. There are situations where the InodeCache
 - of the object file does not get recorded, including a v8 upgrade.
 - There may also be situations where the wrong InodeCache is recorded,
 - if inodes are not stable.
 -
 - This must be called after the content of the object file has been
 - verified to be correct. The InodeCache is generated again to detect if
 - the object file was changed while the content was being verified.
 -}
checkInodeCache :: Key -> OsPath -> Maybe InodeCache -> ActionItem -> Annex ()
checkInodeCache key content mic ai = case mic of
	Nothing -> noop
	Just ic -> do
		ics <- Database.Keys.getInodeCaches key
		unless (null ics) $
			unlessM (isUnmodifiedCheapLowLevel ic ics) $ do
				withTSDelta (liftIO . genInodeCache content) >>= \case
					Nothing -> noop
					Just ic' -> whenM (compareInodeCaches ic ic') $ do
						warning $ actionItemDesc ai
							<> ": Stale or missing inode cache; updating."
						Database.Keys.addInodeCaches key [ic]

checkKeyNumCopies :: Key -> AssociatedFile -> NumCopies -> Annex Bool
checkKeyNumCopies key afile numcopies = do
	let (desc, hasafile) = case afile of
		AssociatedFile Nothing -> (toOsPath (serializeKey'' key), False)
		AssociatedFile (Just af) -> (af, True)
	locs <- loggedLocations key
	(untrustedlocations, otherlocations) <- trustPartition UnTrusted locs
	(deadlocations, safelocations) <- trustPartition DeadTrusted otherlocations
	let present = numCopiesCount safelocations
	if present < fromNumCopies numcopies
		then ifM (checkDead key)
			( do
				showLongNote $ "This key is dead, skipping."
				return True
			, do
				untrusted <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
				dead <- Remote.prettyPrintUUIDs "dead" deadlocations
				warning $ missingNote desc present numcopies untrusted dead
				when (present == 0 && not hasafile) $
					showLongNote "(Avoid this check by running: git annex dead --key )"
				return False
			)
		else return True

missingNote :: OsPath -> Int -> NumCopies -> String -> String -> StringContainingQuotedPath
missingNote file 0 _ [] dead = 
		"** No known copies exist of " <> QuotedPath file <> UnquotedString (honorDead dead)
missingNote file 0 _ untrusted dead =
		"Only these untrusted locations may have copies of " <> QuotedPath file <>
		"\n" <> UnquotedString untrusted <>
		"Back it up to trusted locations with git-annex copy." <> UnquotedString (honorDead dead)
missingNote file present needed [] _ =
		"Only " <> UnquotedString (show present) <> " of " <> UnquotedString (show (fromNumCopies needed)) <>
		" trustworthy copies exist of " <> QuotedPath file <>
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted dead = 
		missingNote file present needed [] dead <>
		"\nThe following untrusted locations may also have copies: " <>
		"\n" <> UnquotedString untrusted
	
honorDead :: String -> String
honorDead dead
	| null dead = ""
	| otherwise = "\nThese dead repositories used to have copies\n" ++ dead

{- Bad content is moved aside. -}
badContent :: Key -> Annex String
badContent key = do
	dest <- moveBad key
	return $ "moved to " ++ fromOsPath dest

{- Bad content is dropped from the remote. We have downloaded a copy
 - from the remote to a temp file already (in some cases, it's just a
 - symlink to a file in the remote). To avoid any further data loss,
 - that temp file is moved to the bad content directory unless 
 - the local annex has a copy of the content. -}
badContentRemote :: Remote -> OsPath -> Key -> Annex String
badContentRemote remote localcopy key = do
	bad <- fromRepo gitAnnexBadDir
	let destbad = bad </> keyFile key
	movedbad <- ifM (inAnnex key <||> liftIO (doesFileExist destbad))
		( return False
		, do
			createAnnexDirectory (parentDir destbad)
			liftIO $ catchDefaultIO False $
				ifM (isSymbolicLink <$> R.getSymbolicLinkStatus (fromOsPath localcopy))
					( copyFileExternal CopyTimeStamps localcopy destbad
					, do
						moveFile localcopy destbad
						return True
					)
		)

	dropped <- tryNonAsync (Remote.removeKey remote Nothing key)
	when (isRight dropped) $
		Remote.logStatus NoLiveUpdate remote key InfoMissing
	return $ case (movedbad, dropped) of
		(True, Right ()) -> "moved from " ++ Remote.name remote ++
			" to " ++ fromOsPath destbad
		(False, Right ()) -> "dropped from " ++ Remote.name remote
		(_, Left e) -> "failed to drop from" ++ Remote.name remote ++ ": " ++ show e

runFsck :: Incremental -> SeekInput -> ActionItem -> Key -> Annex Bool -> CommandStart
runFsck inc si ai key a = stopUnless (needFsck inc key) $
	starting "fsck" (OnlyActionOn key ai) si $ do
		ok <- a
		when ok $
			recordFsckTime inc key
		next $ return ok

{- Check if a key needs to be fscked, with support for incremental fscks. -}
needFsck :: Incremental -> Key -> Annex Bool
needFsck (ScheduleIncremental _ _ i) k = needFsck i k
needFsck (ContIncremental h) key = liftIO $ not <$> FsckDb.inDb h key
needFsck _ _ = return True

recordFsckTime :: Incremental -> Key -> Annex ()
recordFsckTime inc key = withFsckDb inc $ \h -> liftIO $ FsckDb.addDb h key

{- Records the start time of an incremental fsck.
 -
 - To guard against time stamp damage (for example, if an annex directory
 - is copied without -a), the fsckstate file contains a time that should
 - be identical to its modification time.
 - (This is not possible to do on Windows, and so the timestamp in
 - the file will only be equal or greater than the modification time.)
 -}
recordStartTime :: UUID -> Annex ()
recordStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	createAnnexDirectory $ parentDir f
	liftIO $ removeWhenExistsWith removeFile f
	liftIO $ F.withFile f WriteMode $ \h -> do
#ifndef mingw32_HOST_OS
		t <- modificationTime <$> R.getFileStatus (fromOsPath f)
#else
		t <- getPOSIXTime
#endif
		hPutStr h $ showTime $ realToFrac t
	setAnnexFilePerm f
  where
	showTime :: POSIXTime -> String
	showTime = show

resetStartTime :: UUID -> Annex ()
resetStartTime u = liftIO . removeWhenExistsWith removeFile
	=<< fromRepo (gitAnnexFsckState u)

{- Gets the incremental fsck start time. -}
getStartTime :: UUID -> Annex (Maybe EpochTime)
getStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	liftIO $ catchDefaultIO Nothing $ do
		timestamp <- modificationTime <$> R.getFileStatus (fromOsPath f)
		let fromstatus = Just (realToFrac timestamp)
		fromfile <- parsePOSIXTime <$> F.readFile' f
		return $ if matchingtimestamp fromfile fromstatus
			then Just timestamp
			else Nothing
  where
	matchingtimestamp fromfile fromstatus =
#ifndef mingw32_HOST_OS
		fromfile == fromstatus
#else
		fromfile >= fromstatus
#endif

data Incremental
	= NonIncremental (Maybe FsckDb.FsckHandle)
	| ScheduleIncremental Duration UUID Incremental
	| StartIncremental FsckDb.FsckHandle 
	| ContIncremental FsckDb.FsckHandle

prepIncremental :: UUID -> Maybe IncrementalOpt -> Annex Incremental
prepIncremental u (Just StartIncrementalO) = do
	recordStartTime u
	ifM (FsckDb.newPass u)
		( StartIncremental <$> openFsckDb u
		, giveup "Cannot start a new --incremental fsck pass; another fsck process is already running."
		)
prepIncremental u (Just MoreIncrementalO) =
	ContIncremental <$> openFsckDb u
prepIncremental u (Just (ScheduleIncrementalO delta)) = do
	started <- getStartTime u
	i <- prepIncremental u $ Just $ case started of
		Nothing -> StartIncrementalO
		Just _ -> MoreIncrementalO
	return (ScheduleIncremental delta u i)
prepIncremental u Nothing =
	ifM (Annex.getRead Annex.fast)
		-- Avoid recording fscked files in --fast mode,
		-- since that can interfere with a non-fast incremental
		-- fsck.
		( pure (NonIncremental Nothing)
		, (NonIncremental . Just) <$> openFsckDb u
		)

cleanupIncremental :: Incremental -> Annex ()
cleanupIncremental (ScheduleIncremental delta u i) = do
	v <- getStartTime u
	case v of
		Nothing -> noop
		Just started -> do
			now <- liftIO getPOSIXTime
			when (now - realToFrac started >= durationToPOSIXTime delta) $
				resetStartTime u
	cleanupIncremental i
cleanupIncremental _ = return ()

openFsckDb :: UUID -> Annex FsckDb.FsckHandle
openFsckDb u = do
	h <- FsckDb.openDb u
	Annex.addCleanupAction FsckCleanup $
		FsckDb.closeDb h
	return h

withFsckDb :: Incremental -> (FsckDb.FsckHandle -> Annex ()) -> Annex ()
withFsckDb (ContIncremental h) a = a h
withFsckDb (StartIncremental h) a = a h
withFsckDb (NonIncremental mh) a = maybe noop a mh
withFsckDb (ScheduleIncremental _ _ i) a = withFsckDb i a
 
-- A bug caused linked worktrees on filesystems not supporting symlinks
-- to not use the common annex directory, but one annex directory per
-- linked worktree. Object files could end up stored in those directories.
--
-- When run in a linked worktree with its own annex directory that is not a
-- symlink, move any object files to the right location, and delete the
-- annex directory.
cleanupLinkedWorkTreeBug :: Annex ()
cleanupLinkedWorkTreeBug = 
	whenM (Annex.inRepo needsGitLinkFixup) $ do
		r <- Annex.gitRepo
		-- mainWorkTreePath is set by fixupUnusualRepos.
		-- Unsetting it makes a version of the Repo that uses
		-- the wrong object location.
		let r' = r { Git.mainWorkTreePath = Nothing }
		let dir = gitAnnexDir r'
		whenM (liftIO $ dirnotsymlink dir) $ do
			showSideAction $ "Cleaning up directory " 
				<> QuotedPath dir
				<> " created by buggy version of git-annex"
			(st, rd) <- liftIO $ Annex.new' (\r'' _c -> pure r'') r'
			ks <- liftIO $ Annex.eval (st, rd) $
				listKeys InAnnex
			forM_ ks $ \k -> void $ tryNonAsync $ do
				loc <- liftIO $ gitAnnexLocation k r'
					(Annex.gitconfig st)
				moveAnnex k loc
			void $ tryNonAsync $ liftIO $
				removeDirectoryRecursive dir
  where
	dirnotsymlink dir = 
		tryIO (R.getSymbolicLinkStatus (fromOsPath dir)) >>= \case
			Right st -> return $ not (isSymbolicLink st)
			Left _ -> return False
