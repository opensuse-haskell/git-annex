{- git-annex command
 -
 - Copyright 2011, 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Dead where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)
import Logs.Location
import Remote (keyLocations)
import Git.Types

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "dead" SectionSetup "hide a lost repository or key"
		(paramRepeating paramRepository) (seek <$$> optParser)

data DeadOptions = DeadRemotes [RemoteName] | DeadKeys [Key]

optParser :: CmdParamsDesc -> Parser DeadOptions
optParser desc = (DeadRemotes <$> cmdParamsWithCompleter desc completeRemotes)
	<|> (DeadKeys <$> many (option (str >>= parseKey)
		( long "key" <> metavar paramKey
		<> help "keys whose content has been irretrievably lost"
		)))

seek :: DeadOptions -> CommandSeek
seek (DeadRemotes rs) = trustCommand "dead" DeadTrusted rs
seek (DeadKeys ks) = commandActions $ map startKey ks

startKey :: Key -> CommandStart
startKey key = starting "dead" (mkActionItem key) (SeekInput []) $
	keyLocations key >>= \case
		[] -> performKey key
		_ -> giveup "This key is still known to be present in some locations; not marking as dead."

performKey :: Key -> CommandPerform
performKey key = do
	setDead key
	next $ return True
