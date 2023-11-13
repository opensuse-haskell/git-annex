{- git-annex command
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Command.Log where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Time.Clock.POSIX
import Data.Time
import qualified Data.ByteString.Char8 as B8
import qualified System.FilePath.ByteString as P
import Control.Concurrent.Async

import Command
import Logs
import Logs.Location
import Logs.UUID
import qualified Logs.Presence.Pure as PLog
import qualified Annex
import qualified Annex.Branch
import qualified Remote
import qualified Git
import Git.Log
import Git.CatFile
import Utility.DataUnits
import Utility.HumanTime

data LogChange = Added | Removed

type Outputter = LogChange -> POSIXTime -> [UUID] -> Annex ()

cmd :: Command
cmd = withAnnexOptions [jsonOptions, annexedMatchingOptions] $
	command "log" SectionQuery "shows location log"
		paramPaths (seek <$$> optParser)

data LogOptions = LogOptions
	{ logFiles :: CmdParams
	, allOption :: Bool
	, sizesOfOption :: Maybe (DeferredParse UUID)
	, sizesOption :: Bool
	, whenOption :: Maybe Duration
	, rawDateOption :: Bool
	, bytesOption :: Bool
	, gourceOption :: Bool
	, passthruOptions :: [CommandParam]
	}

optParser :: CmdParamsDesc -> Parser LogOptions
optParser desc = LogOptions
	<$> cmdParams desc
	<*> switch
		( long "all"
		<> short 'A'
		<> help "display location log changes to all files"
		)
	<*> optional ((parseUUIDOption <$> strOption
		( long "sizesof"
		<> metavar (paramRemote `paramOr` paramDesc `paramOr` paramUUID)
		<> help "display history of sizes of this repository"
		<> completeRemotes
		)))
	<*> switch
		( long "sizes"
		<> help "display history of sizes of all repositories"
		)
	<*> optional (option (eitherReader parseDuration)
		( long "when" <> metavar paramTime
		<> help "when to display changed size"
		))
	<*> switch
		( long "raw-date"
		<> help "display seconds from unix epoch"
		)
	<*> switch
		( long "bytes"
		<> help "display sizes in bytes"
		)
	<*> switch
		( long "gource"
		<> help "format output for gource"
		)
	<*> (concat <$> many passthru)
  where
	passthru :: Parser [CommandParam]
	passthru = datepassthru "since"
		<|> datepassthru "after"
		<|> datepassthru "until"
		<|> datepassthru "before"
		<|> (mkpassthru "max-count" <$> strOption
			( long "max-count" <> metavar paramNumber
			<> help "limit number of logs displayed"
			))
	datepassthru n = mkpassthru n <$> strOption
		( long n <> metavar paramDate
		<> help ("show log " ++ n ++ " date")
		)
	mkpassthru n v = [Param ("--" ++ n), Param v]

seek :: LogOptions -> CommandSeek
seek o = ifM (null <$> Annex.Branch.getUnmergedRefs)
	( maybe (pure Nothing) (Just <$$> getParsed) (sizesOfOption o) >>= \case
		Just u -> sizeHistoryInfo (Just u) o
		Nothing -> if sizesOption o
			then sizeHistoryInfo Nothing o
			else go	
	, giveup "This repository is read-only, and there are unmerged git-annex branches, which prevents displaying location log changes. (Set annex.merge-annex-branches to false to ignore the unmerged git-annex branches.)"
	)
  where
	ww = WarnUnmatchLsFiles "log"
	go = do
		m <- Remote.uuidDescriptions
		zone <- liftIO getCurrentTimeZone
		outputter <- mkOutputter m zone o <$> jsonOutputEnabled
		let seeker = AnnexedFileSeeker
			{ startAction = start o outputter
			, checkContentPresent = Nothing
			-- the way this uses the location log would not be
			-- helped by precaching the current value
			, usesLocationLog = False
			}
		case (logFiles o, allOption o) of
			(fs, False) -> withFilesInGitAnnex ww seeker
				=<< workTreeItems ww fs
			([], True) -> commandAction (startAll o outputter)
			(_, True) -> giveup "Cannot specify both files and --all"

start :: LogOptions -> (ActionItem -> SeekInput -> Outputter) -> SeekInput -> RawFilePath -> Key -> CommandStart
start o outputter si file key = do
	(changes, cleanup) <- getKeyLog key (passthruOptions o)
	let ai = mkActionItem (file, key)
	showLogIncremental (outputter ai si) changes
	void $ liftIO cleanup
	stop

startAll :: LogOptions -> (ActionItem -> SeekInput -> Outputter) -> CommandStart
startAll o outputter = do
	(changes, cleanup) <- getGitLogAnnex [] (passthruOptions o)
	showLog (\ai -> outputter ai (SeekInput [])) changes
	void $ liftIO cleanup
	stop

{- Displays changes made. Only works when all the RefChanges are for the
 - same key. The method is to compare each value with the value
 - after it in the list, which is the old version of the value.
 -
 - This ncessarily buffers the whole list, so does not stream.
 - But, the number of location log changes for a single key tends to be
 - fairly small.
 -
 - This minimizes the number of reads from git; each logged value is read
 - only once.
 -
 - This also generates subtly better output when the git-annex branch
 - got diverged.
 -}
showLogIncremental :: Outputter -> [RefChange Key] -> Annex ()
showLogIncremental outputter ps = do
	sets <- mapM (getset newref) ps
	previous <- maybe (return genesis) (getset oldref) (lastMaybe ps)
	let l = sets ++ [previous]
	let changes = map (\((t, new), (_, old)) -> (t, new, old))
		(zip l (drop 1 l))
	sequence_ $ compareChanges outputter changes
  where
	genesis = (0, S.empty)
	getset select change = do
		s <- S.fromList <$> loggedLocationsRef (select change)
		return (changetime change, s)

{- Displays changes made. Streams, and can display changes affecting
 - different keys, but does twice as much reading of logged values
 - as showLogIncremental. -}
showLog :: (ActionItem -> Outputter) -> [RefChange Key] -> Annex ()
showLog outputter cs = forM_ cs $ \c -> do
	let ai = mkActionItem (changed c)
	new <- S.fromList <$> loggedLocationsRef (newref c)
	old <- S.fromList <$> loggedLocationsRef (oldref c)
	sequence_ $ compareChanges (outputter ai)
		[(changetime c, new, old)]

mkOutputter :: UUIDDescMap -> TimeZone -> LogOptions -> Bool -> ActionItem -> SeekInput -> Outputter
mkOutputter m zone o jsonenabled ai si
	| jsonenabled = jsonOutput m ai si
	| rawDateOption o = normalOutput lookupdescription ai rawTimeStamp
	| gourceOption o = gourceOutput lookupdescription ai 
	| otherwise = normalOutput lookupdescription ai (showTimeStamp zone rfc822DateFormat)
  where
	lookupdescription u = maybe (fromUUID u) (fromUUIDDesc) (M.lookup u m)

normalOutput :: (UUID -> String) -> ActionItem -> (POSIXTime -> String) -> Outputter
normalOutput lookupdescription ai formattime logchange ts us = do
	qp <- coreQuotePath <$> Annex.getGitConfig
	liftIO $ mapM_ (B8.putStrLn . quote qp . format) us
  where
	time = formattime ts
	addel = case logchange of
		Added -> "+"
		Removed -> "-"
	format u = UnquotedString addel <> " " 
		<> UnquotedString time <> " " 
		<> actionItemDesc ai <> " | " 
		<> UnquotedByteString (fromUUID u) <> " -- "
		<> UnquotedString (lookupdescription u)

jsonOutput :: UUIDDescMap -> ActionItem -> SeekInput -> Outputter
jsonOutput m ai si logchange ts us = do
	showStartMessage $ StartMessage "log" ai si
	maybeShowJSON $ JSONChunk
		[ ("logged", case logchange of
			Added -> "addition"
			Removed -> "removal")
		, ("date", rawTimeStamp ts)
		]
	void $ Remote.prettyPrintUUIDsDescs "locations" m us
	showEndOk

gourceOutput :: (UUID -> String) -> ActionItem -> Outputter
gourceOutput lookupdescription ai logchange ts us =
	liftIO $ mapM_ (putStrLn . intercalate "|" . format) us
  where
	time = takeWhile isDigit $ show ts
	addel = case logchange of
		Added -> "A" 
		Removed -> "M"
	format u =
		[ time
		, lookupdescription u
		, addel
		, decodeBS (noquote (actionItemDesc ai))
		]

{- Generates a display of the changes.
 - Uses a formatter to generate a display of items that are added and
 - removed. -}
compareChanges :: Ord a => (LogChange -> POSIXTime -> [a] -> b) -> [(POSIXTime, S.Set a, S.Set a)] -> [b]
compareChanges format changes = concatMap diff changes
  where
	diff (ts, new, old)
		| new == old = []
		| otherwise = 
			[ format Added ts   $ S.toList $ S.difference new old
			, format Removed ts $ S.toList $ S.difference old new
			]

{- Streams the git log for a given key's location log file.
 -
 - This is complicated by git log using paths relative to the current
 - directory, even when looking at files in a different branch. A wacky
 - relative path to the log file has to be used.
 -
 - The --remove-empty is a significant optimisation. It relies on location
 - log files never being deleted in normal operation. Letting git stop
 - once the location log file is gone avoids it checking all the way back
 - to commit 0 to see if it used to exist, so generally speeds things up a
 - *lot* for newish files. -}
getKeyLog :: Key -> [CommandParam] -> Annex ([RefChange Key], IO Bool)
getKeyLog key os = do
	top <- fromRepo Git.repoPath
	p <- liftIO $ relPathCwdToFile top
	config <- Annex.getGitConfig
	let logfile = p P.</> locationLogFile config key
	getGitLogAnnex [fromRawFilePath logfile] (Param "--remove-empty" : os)

getGitLogAnnex :: [FilePath] -> [CommandParam] -> Annex ([RefChange Key], IO Bool)
getGitLogAnnex fs os = do
	config <- Annex.getGitConfig
	let fileselector = locationLogFileKey config . toRawFilePath
	inRepo $ getGitLog Annex.Branch.fullname fs os fileselector

showTimeStamp :: TimeZone -> String -> POSIXTime -> String
showTimeStamp zone format = formatTime defaultTimeLocale format
	. utcToZonedTime zone . posixSecondsToUTCTime

rawTimeStamp :: POSIXTime -> String
rawTimeStamp t = filter (/= 's') (show t)

sizeHistoryInfo :: (Maybe UUID) -> LogOptions -> Annex ()
sizeHistoryInfo mu o = do
	uuidmap <- getuuidmap
	zone <- liftIO getCurrentTimeZone
	liftIO $ displayheader uuidmap
	let dispst = (zone, False, epoch, Nothing)
	(l, cleanup) <- getlog
	g <- Annex.gitRepo
	liftIO $ catObjectStream g $ \feeder closer reader -> do
		tid <- async $ do
			forM_ l $ \c -> 
				feeder ((changed c, changetime c), newref c)
			closer
		go reader M.empty M.empty M.empty uuidmap dispst
		wait tid
	void $ liftIO cleanup
  where
	-- Go through the log of the git-annex branch in reverse,
	-- and in date order, and pick out changes to location log files
	-- and to the trust log.
	getlog = do
		config <- Annex.getGitConfig
		let fileselector = \f -> let f' = toRawFilePath f in
			case locationLogFileKey config f' of
				Just k -> Just (Right k)
				Nothing
					| f' == trustLog -> Just (Left ())
					| otherwise -> Nothing
		inRepo $ getGitLog Annex.Branch.fullname []
			[ Param "--date-order"
			, Param "--reverse"
			]
			fileselector

	go reader sizemap locmap deadmap uuidmap dispst = reader >>= \case
		Just ((Right k, t), Just logcontent) -> do
			let !newlog = parselocationlog logcontent uuidmap
			let !(sizemap', locmap') = case M.lookup k locmap of
				Nothing -> addnew k sizemap locmap newlog
				Just v -> update k sizemap locmap v newlog
			dispst' <- displaysizes dispst uuidmap sizemap' t
			go reader sizemap' locmap' deadmap uuidmap dispst'
		Just ((Left (), t), Just logcontent) -> do
			-- XXX todo update deadmap
			go reader sizemap locmap deadmap uuidmap dispst
		Just (_, Nothing) -> 
			go reader sizemap locmap deadmap uuidmap dispst
		Nothing -> 
			displayendsizes dispst

	-- Known uuids are stored in this map, and when uuids are stored in the
	-- state, it's a value from this map. This avoids storing multiple
	-- copies of the same uuid in memory.
	getuuidmap = do
		(us, ds) <- unzip . M.toList <$> uuidDescMap
		return $ M.fromList (zip us (zip us ds))
	
	-- Parses a location log file, and replaces the logged uuid
	-- with one from the uuidmap.
	parselocationlog logcontent uuidmap = 
		map replaceuuid $ PLog.parseLog logcontent
	  where
		replaceuuid ll = 
			let !u = toUUID $ PLog.fromLogInfo $ PLog.info ll
			    !ushared = maybe u fst $ M.lookup u uuidmap
			in ll { PLog.info = PLog.LogInfo (fromUUID ushared) }

	presentlocs = map (toUUID . PLog.fromLogInfo . PLog.info)
		. PLog.filterPresent
	
	-- Since the git log is being traversed in date order, commits
	-- from different branches can appear one after the other, and so
	-- the newlog is not necessarily the complete state known at that
	-- time across all git-annex repositories.
	--
	-- This combines the new location log with what has been
	-- accumulated so far, which is equivilant to merging together
	-- all git-annex branches at that point in time.
	update k sizemap locmap (oldlog, oldlocs) newlog = 
		( updatesize (updatesize sizemap sz (S.toList addedlocs))
			(negate sz) (S.toList removedlocs)
		, M.insert k (combinedlog, combinedlocs) locmap
		)
	  where
		sz = ksz k
		combinedlog = PLog.compactLog (oldlog ++ newlog)
		combinedlocs = S.fromList (presentlocs combinedlog)
		addedlocs = S.difference combinedlocs oldlocs
		removedlocs = S.difference oldlocs combinedlocs
	
	addnew k sizemap locmap newlog = 
		( updatesize sizemap (ksz k) locs
		, M.insert k (newlog, S.fromList locs) locmap
		)
	  where
		locs = presentlocs newlog
	
	ksz k = fromMaybe 0 (fromKey keySize k)
	
	updatesize sizemap _ [] = sizemap
	updatesize sizemap sz (l:ls) =
		updatesize (M.insertWith (+) l sz sizemap) sz ls

	epoch = toEnum 0

	displayheader uuidmap
		| sizesOption o = putStrLn $ intercalate "," $
			"date" : map (csvquote . fromUUIDDesc . snd)
				(M.elems uuidmap)
		| otherwise = return ()

	displaysizes (zone, displayedyet, prevt, prevoutput) uuidmap sizemap t
		| t - prevt >= dt
		  && (displayedyet || any (/= 0) sizes) 
		  && (prevoutput /= Just output) = do
			displayts zone t output
			return (zone, True, t, Just output)
		| otherwise = return (zone, displayedyet, prevt, Just output)
	  where
		output = intercalate "," (map showsize sizes)
		us = case mu of
			Just u -> [u]
			Nothing -> M.keys uuidmap
		sizes = map (\u -> fromMaybe 0 (M.lookup u sizemap)) us
		dt = maybe 1 durationToPOSIXTime (whenOption o)

	displayts zone t output = putStrLn $ ts ++ "," ++ output
	  where
		ts = if rawDateOption o
			then rawTimeStamp t
			else showTimeStamp zone "%Y-%m-%dT%H:%M:%S" t

	displayendsizes (zone , _, _, Just output) = do
		now <- getPOSIXTime
		displayts zone now output
	displayendsizes _ = return ()

	showsize n
		| bytesOption o = show n
		| otherwise = roughSize storageUnits True n
	
	csvquote s
		| ',' `elem` s || '"' `elem` s = 
			'"' : concatMap escquote s ++ ['"']
		| otherwise = s
	  where
		escquote '"' = "\"\""
		escquote c = [c]
