{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.UpdateProxy where

import Command
import qualified Annex
import Logs.Proxy
import Logs.Cluster
import Annex.UUID
import qualified Remote as R
import qualified Types.Remote as R
import Annex.SpecialRemote.Config
import Utility.SafeOutput

import qualified Data.Map as M
import qualified Data.Set as S

cmd :: Command
cmd = noMessages $ command "updateproxy" SectionSetup 
	"update records with proxy configuration"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = startingCustomOutput (ActionItemOther Nothing) $ do
	rs <- R.remoteList
	remoteproxies <- S.fromList . map mkproxy
		<$> filterM isproxy rs
	clusterproxies <- getClusterProxies remoteproxies
	let proxies = S.union remoteproxies clusterproxies
	u <- getUUID
	oldproxies <- fromMaybe mempty . M.lookup u <$> getProxies
	if oldproxies == proxies
		then liftIO $ putStrLn "No proxy changes to record."
		else do
			describechanges oldproxies proxies
			recordProxies proxies
	next $ return True
  where
	describechanges oldproxies proxies =
		forM_ (S.toList $ S.union oldproxies proxies) $ \p ->
			case (S.member p oldproxies, S.member p proxies) of
				(False, True) -> liftIO $
					putStrLn $ safeOutput $
						"Started proxying for " ++ proxyRemoteName p
				(True, False) -> liftIO $
					putStrLn $ safeOutput $
						"Stopped proxying for " ++ proxyRemoteName p
				_ -> noop
	
	mkproxy r = Proxy (R.uuid r) (R.name r)
	
	isproxy r
		| remoteAnnexProxy (R.gitconfig r) || not (null (remoteAnnexClusterNode (R.gitconfig r))) = 
			checkCanProxy r "Cannot proxy to this special remote."
		| otherwise = pure False

checkCanProxy :: Remote -> String -> Annex Bool
checkCanProxy r cannotmessage = 
	ifM (R.isExportSupported r)
		( if annexObjects (R.config r)
			then pure True
			else do
				warnannexobjects
				pure False
		, pure True
		)
  where
	warnannexobjects = warning $ UnquotedString $ unwords
		[ R.name r
		, "is configured with exporttree=yes, but without"
		, "annexobjects=yes."
		, cannotmessage
		, "Suggest you run: git-annex enableremote"
		, R.name r
		, "annexobjects=yes"
		]

-- Automatically proxy nodes of any cluster this repository is configured
-- to serve as a gateway for. Also proxy other cluster nodes that are
-- themselves proxied via other remotes.
getClusterProxies :: S.Set Proxy -> Annex (S.Set Proxy)
getClusterProxies remoteproxies = do
	myclusters <- (map mkclusterproxy . M.toList . annexClusters)
		<$> Annex.getGitConfig
	remoteproxiednodes <- findRemoteProxiedClusterNodes
	let myproxieduuids = S.map proxyRemoteUUID remoteproxies 
		<> S.fromList (map proxyRemoteUUID myclusters)
	-- filter out nodes we proxy for from the remote proxied nodes
	-- to avoid cycles
	let remoteproxiednodes' = filter
		(\n -> proxyRemoteUUID n `S.notMember` myproxieduuids)
		remoteproxiednodes
	return (S.fromList (myclusters ++ remoteproxiednodes'))
  where
	mkclusterproxy (remotename, cu) = 
		Proxy (fromClusterUUID cu) remotename

findRemoteProxiedClusterNodes :: Annex [Proxy]
findRemoteProxiedClusterNodes = do
	myclusters <- (S.fromList . M.elems . annexClusters)
		<$> Annex.getGitConfig
	clusternodes <- clusterNodeUUIDs <$> getClusters
	let isproxiedclusternode r
		| isJust (remoteAnnexProxiedBy (R.gitconfig r)) =
			case M.lookup (ClusterNodeUUID (R.uuid r)) clusternodes of
				Nothing -> False
				Just s -> not $ S.null $ 
					S.intersection s myclusters
		| otherwise = False
	(map asproxy . filter isproxiedclusternode)
		<$> R.remoteList
  where
	asproxy r = Proxy (R.uuid r) (R.name r)
