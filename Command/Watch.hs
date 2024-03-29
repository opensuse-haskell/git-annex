{- git-annex watch command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Watch where

import Command
import Assistant
import Utility.HumanTime

cmd :: Command
cmd = notBareRepo $
	command "watch" SectionCommon 
		"daemon to watch for changes and autocommit"
		paramNothing (seek <$$> const (parseDaemonOptions True))

seek :: DaemonOptions -> CommandSeek
seek o = commandAction $ start False o Nothing

start :: Bool -> DaemonOptions -> Maybe Duration -> CommandStart
start assistant o startdelay = do
	if stopDaemonOption o
		then stopDaemon
		else startDaemon assistant 
			(foregroundDaemonOption o)
			startdelay
			Nothing 
			Nothing
			Nothing
			Nothing
			-- does not return
	stop
