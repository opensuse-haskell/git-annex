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

{-# LANGUAGE TupleSections #-}

module Logs.VerifiedRemote (
	remoteConfigMap,
	readRemoteLog,
	configSet,
	checkNeedReadOnly,
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
configSet u c = configSet' [(u, c)]

configSet' :: [(UUID, RemoteConfig)] -> Annex ()
configSet' ucfgs = do
	c <- currentVectorClock
	logf <- fromRepo gitAnnexVerifiedRemoteLog
	lck <- fromRepo gitAnnexVerifiedRemoteLock
	old <- parseRemoteConfigLog . fromMaybe mempty
		<$> readLogFile logf lck
	writeLogFile' logf $ \tmp -> F.writeFile tmp $
		toLazyByteString $ buildRemoteConfigLog $
			foldl' (\v (u, cfg) -> changeLog c u cfg v) old ucfgs
	Annex.changeState $ \s -> s { Annex.verifiedremoteconfigmap = Nothing }

{- Map of remotes by uuid with their RemoteConfigs. Cached for speed.
 -
 - Nothing when the verified remote config log file does not exist yet.
 -}
remoteConfigMap :: Annex (Maybe (M.Map UUID RemoteConfig))
remoteConfigMap =
	maybe load return =<< Annex.getState Annex.verifiedremoteconfigmap
  where
	load = do
		m <- readRemoteLog
		Annex.changeState $ \s ->
			s { Annex.verifiedremoteconfigmap = Just m }
		return m

readRemoteLog :: Annex (Maybe (M.Map UUID RemoteConfig))
readRemoteLog = do
	logf <- fromRepo gitAnnexVerifiedRemoteLog
	lck <- fromRepo gitAnnexVerifiedRemoteLock
	fmap calcRemoteConfigMap <$> readLogFile logf lck

{- Given a list of UUIDs of all currently enabled remotes, check if any of
 - them are not in the verified remote log, in which case all remotes need
 - to be made read-only.
 -
 - This is to prevent exfiltration of data from an unverified remote
 - that accesses a resource that the user did not expect, to an
 - attacker controlled remote.
 - 
 - When remotes need to be made read-only, a message is returned which
 - explains this to the user.
 -}
checkNeedReadOnly
	:: [(UUID, RemoteGitConfig, Maybe RemoteName)]
	-> (M.Map UUID RemoteConfig)
	-- ^ Logs.Remote.remoteConfigMap
	-> Annex (Maybe String)
checkNeedReadOnly rs configmap = remoteConfigMap >>= \case
	Just m -> return $ case mapMaybe (notinmap m) rs of
		[] -> Nothing
		(remotename:_) -> Just $ unwords
			[ "Write prevented due to an unverified special"
			, "remote. Run this command to verify it:"
			, "git-annex verifyremote " ++ remotename
			]
	-- The log file does not exist yet. This is either a newly
	-- initialized repository, or git-annex has been upgraded from a
	-- version that did not use this log.
	--
	-- Handle upgrade by populating the log with the
	-- current configs of all the currently enabled remotes.
	--
	-- In a newly initialized repository, there are not yet
	-- any enabled remotes, and this creates an empty log file.
	Nothing -> do
		configSet' $ mapMaybe (copyfromconfigmap configmap) rs
		return Nothing
  where
	notinmap m (u, gc, remotename)
		| M.notMember u' configmap = Nothing
		| M.member u' m = Nothing
		| otherwise = remotename
	  where
		u' = getcu u gc

	copyfromconfigmap m (u, gc, _) =
		(getcu u gc, ) <$> M.lookup u m
	
	getcu u gc = fromMaybe u (remoteAnnexConfigUUID gc)
