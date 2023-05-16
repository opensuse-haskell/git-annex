{- git-annex command
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Push (cmd) where

import Command
import Command.Sync hiding (cmd)

cmd :: Command
cmd = withAnnexOptions [jobsOption, backendOption] $
	command "push" SectionCommon 
		"push content to remotes"
		(paramRepeating paramRemote) (seek <--< optParser PushMode)
