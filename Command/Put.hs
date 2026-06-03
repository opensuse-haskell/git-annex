{- git-annex command
 -
 - Copyright 2010-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Put where

import Command
import qualified Command.Move
import qualified Remote
import Annex.Wanted
import Annex.NumCopies

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "put" SectionCommon
		"send content of files to other repositories"
		paramPaths (seek <--< optParser)

data PutOptions = PutOptions
	{ putFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, autoMode :: Bool
	, wantedMode :: Bool
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser PutOptions
optParser desc = PutOptions
	<$> cmdParams desc
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseAutoOption
	<*> parseWantedOption
	<*> parseBatchOption True

seek :: PutOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	case batchOption o of
		NoBatch -> withKeyOptions
			(keyOptions o) (autoMode o || wantedMode o) seeker
			(commandAction . keyaction)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (putFiles o)
		Batch fmt -> batchOnly (keyOptions o) (putFiles o) $
			batchAnnexed fmt seeker keyaction
  where
	ww = WarnUnmatchLsFiles "put"
	
	seeker = AnnexedFileSeeker
		{ startAction = startSingle $ const $ start o
		, checkContentPresent = case fto of
			FromOrToRemote (FromRemote _) -> Just False
			FromOrToRemote (ToRemote _) -> Just True
			ToHere -> Just False
			FromRemoteToRemote _ _ -> Nothing
			FromAnywhereToRemote _ -> Nothing
		, usesLocationLog = True
		}
	keyaction = Command.Move.startKey NoLiveUpdate fto Command.Move.RemoveNever

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start :: PutOptions -> FromToHereOptions -> SeekInput -> OsPath -> Key -> CommandStart
start o fto si file key = do
	ru <- case fto of
		FromOrToRemote (ToRemote dest) -> getru dest
		FromOrToRemote (FromRemote _) -> pure Nothing
		ToHere -> pure Nothing
		FromRemoteToRemote _ dest -> getru dest
		FromAnywhereToRemote dest -> getru dest
	lu <- prepareLiveUpdate ru key AddingKey
	start' lu o fto si file key
  where
	getru dest = Just . Remote.uuid <$> getParsed dest

start' :: LiveUpdate -> PutOptions -> FromToHereOptions -> SeekInput -> OsPath -> Key -> CommandStart
start' lu o fto si file key = stopUnless shouldCopy $ 
	Command.Move.start lu fto Command.Move.RemoveNever si file key
  where
	shouldCopy
		| autoMode o = want <||> numCopiesCheck file key (<)
		| wantedMode o = want
		| otherwise = return True
	want = case fto of
		FromOrToRemote (ToRemote dest) -> checkwantsend dest
		FromOrToRemote (FromRemote _) -> checkwantget
		ToHere -> checkwantget
		FromRemoteToRemote _ dest -> checkwantsend dest
		FromAnywhereToRemote dest -> checkwantsend dest

	checkwantsend dest = 
		(Remote.uuid <$> getParsed dest) >>=
			wantGetBy lu False (Just key) (AssociatedFile (Just file))
	checkwantget = wantGet lu False (Just key) (AssociatedFile (Just file))
