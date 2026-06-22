{- git-annex command
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Copy where

import Command
import qualified Annex
import qualified Command.Move
import qualified Remote
import Annex.Wanted
import Annex.NumCopies

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "copy" SectionCommon
		"copy content of files to/from another repository"
		paramPaths (seek <--< optParser)

data CopyOptions = CopyOptions
	{ copyFiles :: CmdParams
	, fromToOptions :: Maybe FromToHereOptions
	, keyOptions :: Maybe KeyOptions
	, autoMode :: Bool
	, wantedMode :: Bool
	, batchOption :: BatchMode
	, moveAction :: Command.Move.MoveAction
	}

optParser :: CmdParamsDesc -> Parser CopyOptions
optParser desc = CopyOptions
	<$> cmdParams desc
	<*> parseFromToHereOptions
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseAutoOption
	<*> parseWantedOption
	<*> parseBatchOption True
	<*> pure (Command.Move.Copy False)

instance DeferredParseClass CopyOptions where
	finishParse v = CopyOptions
		<$> pure (copyFiles v)
		<*> maybe (pure Nothing) (Just <$$> finishParse)
			(fromToOptions v)
		<*> pure (keyOptions v)
		<*> pure (autoMode v)
		<*> pure (wantedMode v)
		<*> pure (batchOption v)
		<*> pure (moveAction v)

seek :: CopyOptions -> CommandSeek
seek o = case fromToOptions o of
	Just fto -> do
		fast <- Annex.getRead Annex.fast
		let o' = o { moveAction = Command.Move.Copy fast }
		seek' o' fto
	Nothing -> giveup "Specify --from or --to"

seek' :: CopyOptions -> FromToHereOptions -> CommandSeek
seek' o fto = startConcurrency (Command.Move.stages fto) $ do
	case batchOption o of
		NoBatch -> withKeyOptions
			(keyOptions o) (autoMode o || wantedMode o) seeker
			(commandAction . startKey o fto id)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (copyFiles o)
		Batch fmt -> batchOnly (keyOptions o) (copyFiles o) $
			batchAnnexed fmt seeker (startKey o fto id)
  where
	ww = WarnUnmatchLsFiles "copy"
	
	seeker = AnnexedFileSeeker
		{ startAction = startSingle $ const $ start o fto id
		, checkContentPresent = case fto of
			FromOrToRemote (FromRemote _) -> Just False
			FromOrToRemote (ToRemote _) -> Just True
			ToHere -> Just False
			FromRemoteToRemote _ _ -> Nothing
			FromAnywhereToRemote _ -> Nothing
		, usesLocationLog = True
		}

{- A copy is just a move that does not delete the source file.
 - However, auto mode avoids unnecessary copies, and avoids getting or
 - sending non-preferred content. -}
start
	:: CopyOptions
	-> FromToHereOptions
	-> (ActionItem -> ActionItem)
	-> SeekInput
	-> OsPath
	-> Key
	-> CommandStart
start o fto fai si file key = do
	ru <- case fto of
		FromOrToRemote (ToRemote dest) -> getru dest
		FromOrToRemote (FromRemote _) -> pure Nothing
		ToHere -> pure Nothing
		FromRemoteToRemote _ dest -> getru dest
		FromAnywhereToRemote dest -> getru dest
	lu <- prepareLiveUpdate ru key AddingKey

	start' lu o fto afile si file key ai
  where
	getru dest = Just . Remote.uuid <$> getParsed dest
	
	afile = AssociatedFile (Just file)
	ai = fai $ mkActionItem (key, afile)

start'
	:: LiveUpdate
	-> CopyOptions
	-> FromToHereOptions
	-> AssociatedFile
	-> SeekInput
	-> OsPath
	-> Key
	-> ActionItem
	-> CommandStart
start' lu o fto afile si file key ai = stopUnless shouldCopy $ 
	Command.Move.start' lu fto (moveAction o) afile si key ai
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

startKey
	:: CopyOptions
	-> FromToHereOptions
	-> (ActionItem -> ActionItem)
	-> (SeekInput, Key, ActionItem)
	-> CommandStart
startKey o fto fai (si, k, ai) = 
	Command.Move.startKey NoLiveUpdate fto (moveAction o) (si, k, fai ai)
