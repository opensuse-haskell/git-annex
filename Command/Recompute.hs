{- git-annex command
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Recompute where

import Command
import qualified Annex
import qualified Remote.Compute
import qualified Remote
import qualified Types.Remote as Remote
import Annex.CatFile
import Annex.Ingest
import Git.FilePath
import Types.KeySource
import Messages.Progress
import Logs.Location
import Utility.Metered
import Backend.URL (fromUrl)
import Command.AddComputed (Reproducible(..), parseReproducible, getInputContent)

import qualified Data.Map as M

cmd :: Command
cmd = notBareRepo $ 
	command "recompute" SectionCommon "recompute computed files"
		paramPaths (seek <$$> optParser)

data RecomputeOptions = RecomputeOptions
	{ recomputeThese :: CmdParams
	, originalOption :: Bool
	, othersOption :: Bool
	, reproducible :: Maybe Reproducible
	, computeRemote :: Maybe (DeferredParse Remote)
	}

optParser :: CmdParamsDesc -> Parser RecomputeOptions
optParser desc = RecomputeOptions
	<$> cmdParams desc
	<*> switch
		( long "original"
		<> help "recompute using original content of input files"
		)
	<*> switch
		( long "others"
		<> help "stage other files that are recomputed in passing"
		)
	<*> parseReproducible
	<*> optional (mkParseRemoteOption <$> parseRemoteOption)

seek :: RecomputeOptions -> CommandSeek
seek o = startConcurrency commandStages (seek' o)

seek' :: RecomputeOptions -> CommandSeek
seek' o = do
	computeremote <- maybe (pure Nothing) (Just <$$> getParsed)
		(computeRemote o)
	let seeker = AnnexedFileSeeker
		{ startAction = const $ start o computeremote
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}
	withFilesInGitAnnex ww seeker
		=<< workTreeItems ww (recomputeThese o)
  where
	ww = WarnUnmatchLsFiles "recompute"

start :: RecomputeOptions -> Maybe Remote -> SeekInput -> OsPath -> Key -> CommandStart
start o (Just computeremote) si file key = 
	stopUnless (notElem (Remote.uuid computeremote) <$> loggedLocations key) $
		start' o computeremote si file key		
start o Nothing si file key = do
	rs <- catMaybes <$> (mapM Remote.byUUID =<< loggedLocations key)
	case sortOn Remote.cost $ filter Remote.Compute.isComputeRemote rs of
		[] -> stop
		(r:_) -> start' o r si file key

start' :: RecomputeOptions -> Remote -> SeekInput -> OsPath -> Key -> CommandStart
start' o r si file key =
	Remote.Compute.getComputeState
		(Remote.remoteStateHandle r) key >>= \case
			Nothing -> stop
			Just state ->
				stopUnless (shouldrecompute state) $
					starting "recompute" ai si $
						perform o r file key state
  where
	ai = mkActionItem (key, file)

	shouldrecompute state
		| originalOption o = return True
		| otherwise = 
			anyM (inputchanged state) $
				M.toList (Remote.Compute.computeInputs state)

	inputchanged state (inputfile, inputkey) = do
		-- Note that the paths from the remote state are not to be
		-- trusted to point to a file in the repository, but using
		-- the path with catKeyFile will only succeed if it
		-- is checked into the repository.
		p <- fromRepo $ fromTopFilePath $ asTopFilePath $
			Remote.Compute.computeSubdir state </> inputfile
		catKeyFile p >>= return . \case
			Just k -> k /= inputkey
			-- When an input file is missing, go ahead and
			-- recompute. This way, the user will see the
			-- computation fail, with an error message that
			-- explains the problem.
			-- XXX check that this works well
			Nothing -> True

perform :: RecomputeOptions -> Remote -> OsPath -> Key -> Remote.Compute.ComputeState -> CommandPerform
perform o r file key oldstate = do
	program <- Remote.Compute.getComputeProgram r
	let recomputestate = oldstate
		{ Remote.Compute.computeInputs = mempty
		, Remote.Compute.computeOutputs = mempty
		}
	fast <- Annex.getRead Annex.fast
	showOutput
	Remote.Compute.runComputeProgram program recomputestate
		(Remote.Compute.ImmutableState False)
		(getinputcontent program fast)
		(go fast)
	next $ return True
  where
	getinputcontent program fast p
		| originalOption o = 
			case M.lookup p (Remote.Compute.computeInputs oldstate) of
				Just inputkey -> return (inputkey, Nothing)
				Nothing -> Remote.Compute.computationBehaviorChangeError program
					"requesting a new input file" p
		| otherwise = getInputContent fast p
	
	go fast state tmpdir ts = do
		let outputs = Remote.Compute.computeOutputs state
		when (M.null outputs) $
			giveup "The computation succeeded, but it did not generate any files."
		oks <- forM (M.keys outputs) $ \outputfile -> do
			showAction $ "adding " <> QuotedPath outputfile
			k <- catchNonAsync (addfile fast state tmpdir outputfile)
				(\err -> giveup $ "Failed to ingest output file " ++ fromOsPath outputfile ++ ": " ++ show err)
			return (outputfile, Just k)
		let state' = state
			{ Remote.Compute.computeOutputs = M.fromList oks
			}
		forM_ (mapMaybe snd oks) $ \k -> do
			Remote.Compute.setComputeState
				(Remote.remoteStateHandle r)
				k ts state'
			logChange NoLiveUpdate k (Remote.uuid r) InfoPresent
	
	addfile fast state tmpdir outputfile
		| fast = do
			addSymlink outputfile stateurlk Nothing
			return stateurlk
		| isreproducible state = do
			sz <- liftIO $ getFileSize outputfile'
			metered Nothing sz Nothing $ \_ p ->
				ingestwith $ ingestAdd p (Just ld)
		| otherwise = ingestwith $
			ingestAdd' nullMeterUpdate (Just ld) (Just stateurlk)
	  where
	  	stateurl = Remote.Compute.computeStateUrl r state outputfile
		stateurlk = fromUrl stateurl Nothing True
		outputfile' = tmpdir </> outputfile
		ld = LockedDown ldc $ KeySource
				{ keyFilename = outputfile
				, contentLocation = outputfile'
				, inodeCache = Nothing
				}
		ingestwith a = a >>= \case
			Nothing -> giveup "key generation failed"
			Just k -> do
				logStatus NoLiveUpdate k InfoPresent
				return k

	ldc = LockDownConfig
		{ lockingFile = True
		, hardlinkFileTmpDir = Nothing
		, checkWritePerms = True
		}
	
	isreproducible state = case reproducible o of
		Just v -> isReproducible v
		Nothing -> Remote.Compute.computeReproducible state
