{- git-annex remote list
 -
 - Copyright 2011-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.List where

import qualified Data.Map as M

import Annex.Common
import qualified Annex
import qualified Logs.Remote
import Logs.VerifiedRemote
import Types.Remote
import Types.RemoteState
import Annex.UUID
import Remote.Helper.Hooks
import Remote.Helper.ReadOnly
import Remote.Helper.ExportImport
import qualified Git

import qualified Remote.Git
import qualified Remote.GCrypt
import qualified Remote.P2P
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Web
import qualified Remote.BitTorrent
import qualified Remote.WebDAV
import qualified Remote.Adb
import qualified Remote.Tahoe
import qualified Remote.Glacier
import qualified Remote.Ddar
import qualified Remote.GitLFS
import qualified Remote.HttpAlso
import qualified Remote.Borg
import qualified Remote.Rclone
import qualified Remote.Hook
import qualified Remote.External
import qualified Remote.Compute
import qualified Remote.Mask

remoteTypes :: [RemoteType]
remoteTypes = map adjustExportImportRemoteType
	[ Remote.Git.remote
	, Remote.GCrypt.remote
	, Remote.P2P.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Web.remote
	, Remote.BitTorrent.remote
	, Remote.WebDAV.remote
	, Remote.Adb.remote
	, Remote.Tahoe.remote
	, Remote.Glacier.remote
	, Remote.Ddar.remote
	, Remote.GitLFS.remote
	, Remote.HttpAlso.remote
	, Remote.Borg.remote
	, Remote.Rclone.remote
	, Remote.Hook.remote
	, Remote.External.remote
	, Remote.Compute.remote
	, Remote.Mask.remote
	]

{- Builds a list of all Remotes.
 - Since doing so can be expensive, the list is cached. -}
remoteList :: Annex [Remote]
remoteList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then remoteList' False
		else return rs

remoteList' :: Bool -> Annex [Remote]
remoteList' autoinit = do
	m <- Logs.Remote.remoteConfigMap
	l <- concat <$> mapM enumerate' remoteTypes

	let usnames = map (\(u, g, gc, _) -> (u, gc, Git.remoteName g)) l
	adjustremote <- mkadjustremote
		<$> unverifiedRemoteAmelioration usnames m
	
	rs <- catMaybes <$> mapM (generate' adjustremote m) l
	Annex.changeState $ \s -> s
		{ Annex.remotes = rs
		, Annex.remotetypes = remoteTypes
		}
	return rs
  where
	enumerate' t = do
		gs <- enumerate t autoinit
		forM gs $ \g -> do
			u <- getRepoUUID g
			gc <- Annex.getRemoteGitConfig g
			return (u, g, gc, t)

	generate' adjustremote m (u, g, gc, t) = 
		remoteGen adjustremote m t g gc u
	
	mkadjustremote Nothing = adjustReadOnly
	mkadjustremote (Just amelioration) = \r ->
		case preventAnnexAccess amelioration (uuid r) of
			Just msg -> preventaccess r msg
			Nothing -> 
				case preventAnnexWrite amelioration of
					Just msg -> forceReadOnly msg r
					Nothing -> adjustReadOnly r

	preventaccess r msg = 
		let ro = forceReadOnly msg r
		in ro
			{ retrieveKeyFile = giveup msg
			, retrieveKeyFileCheap = Nothing
			, lockContent = Nothing
			, checkPresent = giveup msg
			, exportActions = (exportActions ro)
				{ retrieveExport = giveup msg
				, checkPresentExport = giveup msg
				}
			, importActions =  (importActions ro)
				{ listImportableContents = giveup msg
				, importKey = giveup msg
				, retrieveImport = giveup msg
				, checkPresentImport = giveup msg
				}
			, exportImportActions = (exportImportActions ro)
				{ listImportableOrExportedContents = giveup msg
				, importKeyWithContentIdentifier = giveup msg
				, retrieveExportWithContentIdentifier = giveup msg
				, checkPresentExportWithContentIdentifier = giveup msg
				}
			, repairKey = Nothing
			, claimUrl = Nothing
			, checkUrl = Nothing
			}

{- Generates a Remote. -}
remoteGen
	:: (Remote -> Remote)
	-> M.Map UUID RemoteConfig
	-> RemoteType
	-> Git.Repo
	-> RemoteGitConfig
	-> UUID
	-> Annex (Maybe Remote)
remoteGen = remoteGen' id

remoteGen'
	:: (RemoteConfig -> RemoteConfig)
	-> (Remote -> Remote)
	-> M.Map UUID RemoteConfig
	-> RemoteType
	-> Git.Repo
	-> RemoteGitConfig
	-> UUID
	-> Annex (Maybe Remote)
remoteGen' adjustconfig adjustremote m t g gc u = do
	let cu = fromMaybe u $ remoteAnnexConfigUUID gc
	let rs = RemoteStateHandle cu
	let c = adjustconfig (fromMaybe M.empty $ M.lookup cu m)
	generate t g u c gc rs >>= \case
		Nothing -> return Nothing
		Just r -> Just <$> adjustExportImport (adjustremote (addHooks r)) rs

{- Updates a local git Remote, re-reading its git config. -}
updateRemote :: Remote -> Annex (Maybe Remote)
updateRemote remote = do
	m <- Logs.Remote.remoteConfigMap
	g <- updaterepo =<< getRepo remote
	gc <- Annex.getRemoteGitConfig g
	remoteGen adjustReadOnly m (remotetype remote) g gc (uuid remote)
  where
	updaterepo r
		| Git.repoIsLocal r || Git.repoIsLocalUnknown r =
			Remote.Git.configRead False r
		| otherwise = return r

{- Types of remotes that are always syncable using git.
 -
 - This does not include special remotes that may or may not have an
 - annex:: url that allows using git-remote-annex with them.
 -}
gitSyncableRemoteType :: RemoteType -> Bool
gitSyncableRemoteType t = t `elem`
	[ Remote.Git.remote
	, Remote.GCrypt.remote
	, Remote.P2P.remote
	, Remote.GitLFS.remote
	]
