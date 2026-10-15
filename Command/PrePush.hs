{- git-annex command
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.PrePush where

import Command
import Remote.List
import Types.Remote
import qualified Logs.Remote
import qualified Logs.VerifiedRemote

cmd :: Command
cmd = noCommit $ command "pre-push" SectionPlumbing
	"run by git pre-push hook"
	paramPaths
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = do
	rs <- map (\r -> (uuid r, gitconfig r, Just (name r))) <$> remoteList
	m <- Logs.Remote.remoteConfigMap
	maybe noop giveup
		=<< Logs.VerifiedRemote.checkNeedReadOnly rs m "Push"
