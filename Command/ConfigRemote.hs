{- git-annex command
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.ConfigRemote where

import Command
import qualified Logs.Remote
import qualified Git.Types as Git
import qualified Annex.SpecialRemote as SpecialRemote
import qualified Types.Remote as Remote
import Types.ProposedAccepted
import Command.EnableRemote (unknownNameError, startSpecialRemote', PerformSpecialRemote, deadLast)

import qualified Data.Map as M

cmd :: Command
cmd = command "configremote" SectionSetup
	"changes special remote configuration"
	(paramPair paramName $ paramOptional $ paramRepeating paramParamValue)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start [] = unknownNameError "Specify the remote to configure."
start (name:rest) = do
	cfg <- safeConfig rest
	deadLast name $ startSpecialRemote name cfg

{- Since this command stores config without calling the remote's setup
 - method to validate it, it can only be used on fields that are known to
 - be safe to change in all remotes. -}
safeConfig :: [String] -> Annex Remote.RemoteConfig
safeConfig cs = do
	let rc = Logs.Remote.keyValToConfig Proposed cs
	forM_ (M.keys rc) $ \k ->
		when (fromProposedAccepted k `notElem` safefields) $
			giveup $ "Cannot change field \"" ++ fromProposedAccepted k  ++ "\" with this command. Use git-annex enableremote instead."
	case SpecialRemote.parseRemoteConfig rc (Remote.RemoteConfigParser ps Nothing) of
		Left err -> giveup err
		Right _ -> return rc
  where
	ps = [ SpecialRemote.autoEnableFieldParser ]
	safefields = [ fromProposedAccepted SpecialRemote.autoEnableField ]

startSpecialRemote :: Git.RemoteName -> Remote.RemoteConfig -> [(UUID, Remote.RemoteConfig, Maybe (SpecialRemote.ConfigFrom UUID))] -> CommandStart
startSpecialRemote = startSpecialRemote' "configremote" performSpecialRemote

performSpecialRemote :: PerformSpecialRemote
performSpecialRemote _ u _ c _ mcu = do
	case mcu of
		Nothing -> Logs.Remote.configSet u c
		Just (SpecialRemote.ConfigFrom cu) ->
			Logs.Remote.configSet cu c
	next $ return True
