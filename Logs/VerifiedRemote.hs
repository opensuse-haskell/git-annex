{- git-annex verified remote log
 - 
 - This contains the same information as the git-annex remote log,
 - but only remote configs that have been verified by the user to be safe
 - to use.
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.VerifiedRemote (
	readRemoteLog,
	configSet,
) where

import Annex.Common
import qualified Annex
import Types.Remote
import Logs.File
import Logs.UUIDBased
import Logs.Remote.Pure
import Annex.SpecialRemote.Config
import qualified Utility.FileIO as F

import qualified Data.Map as M
import Data.ByteString.Builder

configSet :: UUID -> RemoteConfig -> Annex ()
configSet u cfg = do
	c <- currentVectorClock
	logf <- fromRepo gitAnnexVerifiedRemoteLog
	lck <- fromRepo gitAnnexVerifiedRemoteLock
	old <- parseRemoteConfigLog . fromMaybe mempty
		<$> readLogFile logf lck
	writeLogFile' logf $ \tmp -> F.writeFile tmp $
		toLazyByteString $ buildRemoteConfigLog $
			changeLog c u (removeSameasInherited cfg) old

readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = do
	logf <- fromRepo gitAnnexVerifiedRemoteLog
	lck <- fromRepo gitAnnexVerifiedRemoteLock
	maybe mempty calcRemoteConfigMap <$> readLogFile logf lck
