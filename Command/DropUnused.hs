{- git-annex command
 -
 - Copyright 2010,2012,2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.DropUnused where

import Command
import qualified Annex
import qualified Command.Drop
import qualified Remote
import qualified Git
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)
import Annex.NumCopies
import Annex.Content

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions] $
	command "dropunused" SectionMaintenance
		"drop unused file content"
		(paramRepeating paramNumRange) (seek <$$> optParser)

data DropUnusedOptions = DropUnusedOptions
	{ rangesToDrop :: CmdParams
	, dropFrom :: Maybe (DeferredParse Remote)
	}

optParser :: CmdParamsDesc -> Parser DropUnusedOptions
optParser desc = DropUnusedOptions
	<$> cmdParams desc
	<*> optional (Command.Drop.parseDropFromOption)

seek :: DropUnusedOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	numcopies <- getNumCopies
	mincopies <- getMinCopies
	from <- maybe (pure Nothing) (Just <$$> getParsed) (dropFrom o)
	withUnusedMaps (start from numcopies mincopies) (rangesToDrop o)

start :: Maybe Remote -> NumCopies -> MinCopies -> UnusedMaps -> Int -> CommandStart
start from numcopies mincopies = startUnused
	(go (perform from numcopies mincopies))
	(go (performOther gitAnnexBadLocation))
	(go (performOther gitAnnexTmpObjectLocation))
  where
	go a n key = starting "dropunused" 
		(ActionItemOther $ Just $ UnquotedString $ show n)
		(SeekInput [show n])
		(a key)

perform :: Maybe Remote -> NumCopies -> MinCopies -> Key -> CommandPerform
perform from numcopies mincopies key = case from of
	Just r -> do
		showAction $ UnquotedString $ "from " ++ Remote.name r
		Command.Drop.performRemote NoLiveUpdate pcc key
			(AssociatedFile Nothing) numcopies mincopies r ud
	Nothing -> ifM (inAnnex key)
		( droplocal
		, ifM (objectFileExists key)
			( ifM (Annex.getRead Annex.force)
				( droplocal
				, do
					warning "Annexed object has been modified and dropping it would probably lose the only copy. Run this command with --force if you want to drop it anyway."
					next $ return False
				)
			, next $ return True
			)
		)
  where
	droplocal = Command.Drop.performLocal NoLiveUpdate pcc 
		key (AssociatedFile Nothing) numcopies mincopies [] ud
	pcc = Command.Drop.PreferredContentChecked False
	ud = Command.Drop.DroppingUnused True

performOther :: (Key -> Git.Repo -> OsPath) -> Key -> CommandPerform
performOther filespec key = do
	f <- fromRepo $ filespec key
	pruneTmpWorkDirBefore f (liftIO . removeWhenExistsWith removeFile)
	next $ return True
