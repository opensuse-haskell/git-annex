{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RecvKey where

import Command
import Annex.Content
import Annex.Action
import Annex
import Utility.Rsync
import Types.Transfer
import Logs.Location
import Command.SendKey (fieldTransfer)
import qualified CmdLine.GitAnnexShell.Fields as Fields

cmd :: Command
cmd = noCommit $ command "recvkey" SectionPlumbing 
	"runs rsync in server mode to receive content"
	paramKey (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys (commandAction . start)

start :: (SeekInput, Key) -> CommandStart
start (_, key) = fieldTransfer Download key $ \_p -> do
	-- Always verify content when a repo is sending an unlocked file,
	-- as the file could change while being transferred.
	fromunlocked <- (isJust <$> Fields.getField Fields.unlocked)
		<||> (isJust <$> Fields.getField Fields.direct)
	let verify = if fromunlocked then AlwaysVerify else DefaultVerify
	-- This matches the retrievalSecurityPolicy of Remote.Git
	let rsp = RetrievalAllKeysSecure
	ifM (getViaTmp rsp verify key (AssociatedFile Nothing) go)
		( do
			logStatus key InfoPresent
			-- forcibly quit after receiving one key,
			-- and shutdown cleanly
			_ <- shutdown True
			return True
		, return False
		)
  where
	go tmp = unVerified $ do
		opts <- filterRsyncSafeOptions . maybe [] words
			<$> getField "RsyncOptions"
		liftIO $ rsyncServerReceive (map Param opts) (fromRawFilePath tmp)
