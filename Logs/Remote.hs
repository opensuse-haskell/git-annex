{- git-annex remote log
 - 
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Remote (
	remoteLog,
	remoteConfigMap,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,
	showConfig,

	prop_isomorphic_configEscape,
	prop_parse_show_Config,
) where

import Annex.Common
import qualified Annex
import qualified Annex.Branch
import Types.Remote
import Logs
import Logs.UUIDBased
import Logs.Remote.Pure
import qualified Logs.VerifiedRemote
import Annex.SpecialRemote.Config

import qualified Data.Map as M

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u cfg = do
	c <- currentVectorClock
	Annex.Branch.change (Annex.Branch.RegardingUUID [u]) remoteLog $
		buildRemoteConfigLog
			. changeLog c u (removeSameasInherited cfg)
			. parseRemoteConfigLog
	Annex.changeState $ \s -> s { Annex.remoteconfigmap = Nothing }

{- Map of remotes by uuid with their RemoteConfigs. Cached for speed.
 -
 - When the verified remote log has a RemoteConfig, it is used, otherwise
 - the value from the git-annex branch is used. This ensures that, once a
 - user has verified a RemoteConfig, it does not change unexpectedly.
 -}
remoteConfigMap :: Annex (M.Map UUID RemoteConfig)
remoteConfigMap = maybe load return =<< Annex.getState Annex.remoteconfigmap
  where
	load = do
		logged <- readRemoteLog
		verified <- fromMaybe mempty
			<$> Logs.VerifiedRemote.remoteConfigMap
		let m = M.union verified logged
		Annex.changeState $ \s ->
			s { Annex.remoteconfigmap = Just m }
		return m

readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = calcRemoteConfigMap 
	<$> Annex.Branch.get remoteLog
