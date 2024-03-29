{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.PostReceive where

import Command
import qualified Annex
import Git.Types
import Annex.UpdateInstead
import Annex.CurrentBranch
import Command.Sync (mergeLocal, prepMerge, mergeConfig, SyncOptions(..))

-- This does not need to modify the git-annex branch to update the 
-- work tree, but auto-initialization might change the git-annex branch.
-- Since it would be surprising for a post-receive hook to make such a
-- change, that's prevented by noCommit.
cmd :: Command
cmd = noCommit $
	command "post-receive" SectionPlumbing
		"run by git post-receive hook"
		paramNothing
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = whenM needUpdateInsteadEmulation $ do
	fixPostReceiveHookEnv
	commandAction updateInsteadEmulation

{- When run by the post-receive hook, the cwd is the .git directory, 
 - and GIT_DIR=. It's not clear why git does this.
 -
 - Fix up from that unusual situation, so that git commands
 - won't try to treat .git as the work tree. -}
fixPostReceiveHookEnv :: Annex ()
fixPostReceiveHookEnv = do
	g <- Annex.gitRepo
	case location g of
		Local { gitdir = ".", worktree = Just "." } ->
			Annex.adjustGitRepo $ \g' -> pure $ g'
				{ location = case location g' of
					loc@(Local {}) -> loc 
						{ worktree = Just ".." }
					loc -> loc
				}
		_ -> noop

updateInsteadEmulation :: CommandStart
updateInsteadEmulation = do
	prepMerge
	let o = def { notOnlyAnnexOption = True }
	mc <- mergeConfig False
	mergeLocal mc o =<< getCurrentBranch
