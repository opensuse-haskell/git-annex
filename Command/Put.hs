{- git-annex command
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Put where

import Command
import qualified Command.Copy
import qualified Remote

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "put" SectionCommon
		"send content of files to other repositories"
		paramPaths (seek <$$> optParser)

data PutOptions = PutOptions
	{ putFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, wantedMode :: Bool
	, autoMode :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser PutOptions
optParser desc = PutOptions
	<$> cmdParams desc
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseWantedOption
	<*> parseAutoOption
	<*> parseBatchOption True

seek :: PutOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	contentremotes <- filter Remote.canPut
		. concat . Remote.byCost 
		<$> (Remote.contentRemotes =<< Remote.remoteList)
	let seeker = AnnexedFileSeeker
		{ startAction = \_ si p k ->
			return $ flip map contentremotes $ \r ->
				Command.Copy.start co (to r) si p k
		, checkContentPresent = Just True
		, usesLocationLog = True
		}
	let keyaction v = 
		return $ flip map contentremotes $ \r ->
			Command.Copy.startKey (to r) v
	case batchOption o of
		NoBatch -> withKeyOptions
			(keyOptions o) (autoMode o || wantedMode o) seeker
			(\v -> commandActions =<< keyaction v)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (putFiles o)
		Batch fmt -> batchOnly (keyOptions o) (putFiles o) $
			batchAnnexed' fmt seeker keyaction
  where
	ww = WarnUnmatchLsFiles "put"

	co = Command.Copy.CopyOptions
		{ Command.Copy.copyFiles = putFiles o
		, Command.Copy.fromToOptions = Nothing
		, Command.Copy.keyOptions = keyOptions o
		, Command.Copy.autoMode = autoMode o
		, Command.Copy.wantedMode = wantedMode o
		, Command.Copy.batchOption = batchOption o
		}
	
	to = FromOrToRemote . ToRemote . ReadyParse
