{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Reinit where

import Command
import Annex.Init
import Annex.UUID
import Annex.Startup
import qualified Remote
import qualified Annex.SpecialRemote
	
cmd :: Command
cmd = dontCheck repoExists $ withAnnexOptions [jsonOptions] $
	command "reinit" SectionUtility 
		"initialize repository, reusing old UUID"
		(paramUUID ++ "|" ++ paramDesc)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start ws = starting "reinit" ai (SeekInput ws) $
	perform s
  where
	ai = ActionItemOther (Just (UnquotedString s))
	s = unwords ws

perform :: String -> CommandPerform
perform s = do
	u <- if isUUID s
		then return $ toUUID s
		else Remote.nameToUUID s
	storeUUID u
	checkInitializeAllowed $ initialize' startupAnnex Nothing
	Annex.SpecialRemote.autoEnable
	next $ return True
