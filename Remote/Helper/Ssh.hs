{- git-annex remote access with ssh and git-annex-shell
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Helper.Ssh where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.Url
import Annex.UUID
import Annex.Ssh
import CmdLine.GitAnnexShell.Fields (Field, fieldName)
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Remote.Helper.Messages
import Utility.Metered
import Utility.Rsync
import Utility.SshHost
import Utility.Debug
import Types.Remote
import Types.Transfer
import Config
import qualified P2P.Protocol as P2P
import qualified P2P.IO as P2P
import qualified P2P.Annex as P2P

import Control.Concurrent.STM

toRepo :: ConsumeStdin -> Git.Repo -> RemoteGitConfig -> SshCommand -> Annex (FilePath, [CommandParam])
toRepo cs r gc remotecmd = do
	let host = maybe
		(giveup "bad ssh url")
		(either giveup id . mkSshHost)
		(Git.Url.hostuser r)
	sshCommand cs (host, Git.Url.port r) gc remotecmd

{- Generates parameters to run a git-annex-shell command on a remote
 - repository. -}
git_annex_shell :: ConsumeStdin -> Git.Repo -> String -> [CommandParam] -> [(Field, String)] -> Annex (Maybe (FilePath, [CommandParam]))
git_annex_shell cs r command params fields
	| not $ Git.repoIsUrl r = do
		dir <- liftIO $ absPath (Git.repoPath r)
		shellopts <- getshellopts dir
		return $ Just (shellcmd, shellopts ++ fieldopts)
	| Git.repoIsSsh r = do
		gc <- Annex.getRemoteGitConfig r
		u <- getRepoUUID r
		shellopts <- getshellopts (Git.repoPath r)
		let sshcmd = unwords $
			fromMaybe shellcmd (remoteAnnexShell gc)
				: map shellEscape (toCommand shellopts) ++
			uuidcheck u ++
			map shellEscape (toCommand fieldopts)
		Just <$> toRepo cs r gc sshcmd
	| otherwise = return Nothing
  where
	shellcmd = "git-annex-shell"
	getshellopts dir = do
		debugenabled <- Annex.getRead Annex.debugenabled
		debugselector <- Annex.getRead Annex.debugselector
		let params' = case (debugenabled, debugselector) of
			(True, NoDebugSelector) -> Param "--debug" : params
			_ -> params
		return (Param command : File (fromOsPath dir) : params')
	uuidcheck NoUUID = []
	uuidcheck u@(UUID _) = ["--uuid", fromUUID u]
	fieldopts
		| null fields = []
		| otherwise = fieldsep : map fieldopt fields ++ [fieldsep]
	fieldsep = Param "--"
	fieldopt (field, value) = Param $
		fieldName field ++ "=" ++ value

{- Uses a supplied function (such as boolSystem) to run a git-annex-shell
 - command on a remote.
 -
 - Or, if the remote does not support running remote commands, returns
 - a specified error value. -}
onRemote 
	:: ConsumeStdin
	-> Git.Repo
	-> (FilePath -> [CommandParam] -> Annex a, Annex a)
	-> String
	-> [CommandParam]
	-> [(Field, String)]
	-> Annex a
onRemote cs r (with, errorval) command params fields = do
	s <- git_annex_shell cs r command params fields
	case s of
		Just (c, ps) -> with c ps
		Nothing -> errorval

{- Checks if a remote contains a key. -}
inAnnex :: Git.Repo -> Key -> Annex Bool
inAnnex r k = onRemote NoConsumeStdin r (runcheck, cantCheck r) "inannex"
	[Param $ serializeKey k] []
  where
	runcheck c p = liftIO $ dispatch =<< safeSystem c p
	dispatch ExitSuccess = return True
	dispatch (ExitFailure 1) = return False
	dispatch _ = cantCheck r

{- Removes a key from a remote using the legacy git-annex-shell dropkey,
 - rather than the P2P protocol.
 -
 - The proof is not checked for expire on the remote, so this should only
 - be used by remotes that do not have lockContent return a LockedCopy. -}
dropKey :: Git.Repo -> Maybe SafeDropProof -> Key -> Annex ()
dropKey r proof key = unlessM (dropKey' r proof key) $
	giveup "unable to remove key from remote"

dropKey' :: Git.Repo -> Maybe SafeDropProof -> Key -> Annex Bool
dropKey' r _proof key = onRemote NoConsumeStdin r (\f p -> liftIO (boolSystem f p), return False) "dropkey"
	[ Param "--quiet", Param "--force"
	, Param $ serializeKey key
	]
	[]

rsyncHelper :: OutputHandler -> Maybe MeterUpdate -> [CommandParam] -> Annex Bool
rsyncHelper oh m params = do
	unless (quietMode oh) $
		showOutput -- make way for progress bar
	a <- case m of
		Nothing -> return $ rsync params
		Just meter -> return $ rsyncProgress oh meter params
	ifM (liftIO a)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParamsRemote :: Remote -> Direction -> Key -> FilePath -> Annex [CommandParam]
rsyncParamsRemote r direction key file = do
	u <- getUUID
	repo <- getRepo r
	Just (shellcmd, shellparams) <- git_annex_shell ConsumeStdin repo
		(if direction == Download then "sendkey" else "recvkey")
		[ Param $ serializeKey key ]
		[(Fields.remoteUUID, fromUUID u)]
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	o <- rsyncParams r direction
	return $ if direction == Download
		then o ++ rsyncopts eparam dummy (File file)
		else o ++ rsyncopts eparam (File file) dummy
  where
	rsyncopts ps source dest
		| end ps == [dashdash] = ps ++ [source, dest]
		| otherwise = ps ++ [dashdash, source, dest]
	dashdash = Param "--"
	{- The rsync shell parameter controls where rsync
	 - goes, so the source/dest parameter can be a dummy value,
	 - that just enables remote rsync mode.
	 - For maximum compatibility with some patched rsyncs,
	 - the dummy value needs to still contain a hostname,
	 - even though this hostname will never be used. -}
	dummy = Param "dummy:"

-- --inplace to resume partial files
--
-- Only use --perms when not on a crippled file system, as rsync
-- will fail trying to restore file perms onto a filesystem that does not
-- support them.
rsyncParams :: Remote -> Direction -> Annex [CommandParam]
rsyncParams r direction = do
	crippled <- crippledFileSystem
	return $ map Param $ catMaybes
		[ Just "--progress"
		, Just "--inplace"
		, if crippled then Nothing else Just "--perms"
		] 
		++ remoteAnnexRsyncOptions gc ++ dps
  where
	dps
		| direction == Download = remoteAnnexRsyncDownloadOptions gc
		| otherwise = remoteAnnexRsyncUploadOptions gc
	gc = gitconfig r

-- A connection over ssh or locally to git-annex shell,
-- speaking the P2P protocol.
type P2PShellConnection = P2P.ClosableConnection 
	(P2P.RunState, P2P.P2PConnection, ProcessHandle)

closeP2PShellConnection :: P2PShellConnection -> IO (P2PShellConnection, Maybe ExitCode)
closeP2PShellConnection P2P.ClosedConnection = return (P2P.ClosedConnection, Nothing)
closeP2PShellConnection (P2P.OpenConnection (_st, conn, pid)) =
	-- mask async exceptions, avoid cleanup being interrupted
	uninterruptibleMask_ $ do
		P2P.closeConnection conn
		exitcode <- waitForProcess pid
		return (P2P.ClosedConnection, Just exitcode)

-- Pool of connections to git-annex-shell p2pstdio.
type P2PShellConnectionPool = TVar (Maybe P2PShellConnectionPoolState)

data P2PShellConnectionPoolState
	= P2PShellConnections [P2PShellConnection]
	-- Remotes using an old version of git-annex-shell don't support P2P
	| P2PShellUnsupported

mkP2PShellConnectionPool :: Annex P2PShellConnectionPool
mkP2PShellConnectionPool = liftIO $ newTVarIO Nothing

-- Takes a connection from the pool, if any are available, otherwise
-- tries to open a new one.
getP2PShellConnection :: Remote -> P2PShellConnectionPool -> Annex (Maybe P2PShellConnection)
getP2PShellConnection r connpool = getexistingconn >>= \case
	Nothing -> return Nothing
	Just Nothing -> openP2PShellConnection r connpool
	Just (Just c) -> return (Just c)
  where
	getexistingconn = liftIO $ atomically $ readTVar connpool >>= \case
		Just P2PShellUnsupported -> return Nothing
		Just (P2PShellConnections (c:cs)) -> do
			writeTVar connpool (Just (P2PShellConnections cs))
			return (Just (Just c))
		Just (P2PShellConnections []) -> return (Just Nothing)
		Nothing -> return (Just Nothing)

-- Add a connection to the pool, unless it's closed.
storeP2PShellConnection :: P2PShellConnectionPool -> P2PShellConnection -> IO ()
storeP2PShellConnection _ P2P.ClosedConnection = return ()
storeP2PShellConnection connpool conn = atomically $ modifyTVar' connpool $ \case
	Just (P2PShellConnections cs) -> Just (P2PShellConnections (conn:cs))
	_ -> Just (P2PShellConnections [conn])

-- Try to open a P2PShellConnection.
-- The new connection is not added to the pool, so it's available
-- for the caller to use.
-- If the remote does not support the P2P protocol, that's remembered in 
-- the connection pool.
openP2PShellConnection :: Remote -> P2PShellConnectionPool -> Annex (Maybe P2PShellConnection)
openP2PShellConnection r connpool =
	openP2PShellConnection' r P2P.maxProtocolVersion mempty >>= \case
		Just conn -> return (Just conn)
		Nothing -> do
			liftIO $ rememberunsupported
			return Nothing
  where
	rememberunsupported = atomically $
		modifyTVar' connpool $
			maybe (Just P2PShellUnsupported) Just

openP2PShellConnection' :: Remote -> P2P.ProtocolVersion -> P2P.Bypass -> Annex (Maybe P2PShellConnection)
openP2PShellConnection' r maxprotoversion bypass = do
	u <- getUUID
	let ps = [Param (fromUUID u)]
	repo <- getRepo r
	git_annex_shell ConsumeStdin repo "p2pstdio" ps [] >>= \case
		Nothing -> return Nothing
		Just (cmd, params) -> start cmd params
  where
	start cmd params = liftIO $ do
		(Just from, Just to, Nothing, pid) <- createProcess $
			(proc cmd (toCommand params))
				{ std_in = CreatePipe
				, std_out = CreatePipe
				}
		pidnum <- getPid pid
		let conn = P2P.P2PConnection
			{ P2P.connRepo = Nothing
			, P2P.connCheckAuth = const False
			, P2P.connIhdl = P2P.P2PHandle to
			, P2P.connOhdl = P2P.P2PHandle from
			, P2P.connIdent = P2P.ConnIdent $
				Just $ "git-annex-shell connection " ++ show pidnum
			}
		runst <- P2P.mkRunState P2P.Client
		let c = P2P.OpenConnection (runst, conn, pid)
		-- When the connection is successful, the remote
		-- will send an AUTH_SUCCESS with its uuid.
		let proto = P2P.postAuth $ do
			P2P.negotiateProtocolVersion maxprotoversion
			P2P.sendBypass bypass
		tryNonAsync (P2P.runNetProto runst conn proto) >>= \case
			Right (Right (Just theiruuid)) | theiruuid == uuid r ->
				return $ Just c
			_ -> do
				(cclosed, exitcode) <- closeP2PShellConnection c
				-- ssh exits 255 when unable to connect to
				-- server.
				if exitcode == Just (ExitFailure 255)
					then return (Just cclosed)
					else return Nothing

-- Runs a P2P Proto action on a remote when it supports that,
-- otherwise the fallback action.
runProto :: Remote -> P2PShellConnectionPool -> Annex a -> P2P.Proto a -> Annex (Maybe a)
runProto r connpool onerr proto = Just <$>
	(getP2PShellConnection r connpool >>= maybe onerr go)
  where
	go c = do
		(c', v) <- runProtoConn proto c
		case v of
			Just res -> do
				liftIO $ storeP2PShellConnection connpool c'
				return res
			Nothing -> onerr

runProtoConn :: P2P.Proto a -> P2PShellConnection -> Annex (P2PShellConnection, Maybe a)
runProtoConn _ P2P.ClosedConnection = return (P2P.ClosedConnection, Nothing)
runProtoConn a conn@(P2P.OpenConnection (runst, c, _)) = do
	P2P.runFullProto runst c a >>= \case
		Right r -> return (conn, Just r)
		-- When runFullProto fails, the connection is no longer
		-- usable, so close it.
		Left e -> do
			warning $ UnquotedString $ "Lost connection (" ++ P2P.describeProtoFailure e ++ ")"
			conn' <- fst <$> liftIO (closeP2PShellConnection conn)
			return (conn', Nothing)

-- Allocates a P2P shell connection from the pool, and runs the action with
-- it, returning the connection to the pool once the action is done.
--
-- If the remote does not support the P2P protocol, runs the fallback
-- action instead.
withP2PShellConnection
	:: Remote
	-> P2PShellConnectionPool
	-> Annex a
	-> (P2PShellConnection -> Annex (P2PShellConnection, a))
	-> Annex a
withP2PShellConnection r connpool fallback a = bracketOnError get cache go
  where
	get = getP2PShellConnection r connpool
	cache (Just conn) = liftIO $ storeP2PShellConnection connpool conn
	cache Nothing = return ()
	go (Just conn) = do
		(conn', res) <- a conn
		cache (Just conn')
		return res
	go Nothing = fallback
