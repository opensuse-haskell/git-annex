{- git remotes encrypted using git-remote-gcrypt
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.GCrypt (remote, gen, getGCryptId) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

import Common.Annex
import Types.Remote
import Types.GitConfig
import Types.Crypto
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.GCrypt
import qualified Git.Construct
import qualified Git.Types as Git ()
import qualified Annex.Branch
import qualified Annex.Content
import Config
import Config.Cost
import Remote.Helper.Git
import Remote.Helper.Encryptable
import Remote.Helper.Special
import Utility.Metered
import Crypto
import Annex.UUID
import Annex.Ssh
import qualified Remote.Rsync
import Utility.Rsync
import Logs.Remote
import Utility.Gpg

remote :: RemoteType
remote = RemoteType {
	typename = "gcrypt",
	-- Remote.Git takes care of enumerating gcrypt remotes too,
	-- and will call our gen on them.
	enumerate = return [],
	generate = gen,
	setup = gCryptSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen gcryptr u c gc = do
	g <- gitRepo
	-- get underlying git repo with real path, not gcrypt path
	r <- liftIO $ Git.GCrypt.encryptedRepo g gcryptr
	let r' = r { Git.remoteName = Git.remoteName gcryptr }
	(mgcryptid, r'') <- liftIO $ getGCryptId r'
	-- doublecheck that local cache matches underlying repo's gcrypt-id
	-- (which might not be set)
	case (mgcryptid, Git.GCrypt.remoteRepoId g (Git.remoteName gcryptr)) of
		(Just gcryptid, Just cachedgcryptid)
			| gcryptid /= cachedgcryptid -> resetup gcryptid r''
		_ -> gen' r'' u c gc
  where
	-- A different drive may have been mounted, making a different
	-- gcrypt remote available. So need to set the cached
	-- gcrypt-id and annex-uuid of the remote to match the remote
	-- that is now available. Also need to set the gcrypt particiants
	-- correctly.
	resetup gcryptid r = do
		let u' = genUUIDInNameSpace gCryptNameSpace gcryptid
		v <- (M.lookup u' <$> readRemoteLog)
		case (Git.remoteName gcryptr, v) of
			(Just remotename, Just c') -> do
				setGcryptEncryption c' remotename
				setConfig (remoteConfig gcryptr "uuid") (fromUUID u')
				setConfig (ConfigKey $ Git.GCrypt.remoteConfigKey "gcrypt-id" remotename) gcryptid
				gen' r u' c' gc
			_ -> do
				warning $ "not using unknown gcrypt repository pointed to by remote " ++ Git.repoDescribe r
				return Nothing

{- gcrypt repos set up by git-annex as special remotes have a
 - core.gcrypt-id setting in their config, which can be mapped back to
 - the remote's UUID. This only works for local repos.
 - (Also returns a version of input repo with its config read.) -}
getGCryptId :: Git.Repo -> IO (Maybe Git.GCrypt.GCryptId, Git.Repo)
getGCryptId r
	| Git.repoIsLocalUnknown r = do
		r' <- catchDefaultIO r $ Git.Config.read r
		return (Git.Config.getMaybe "core.gcrypt-id" r', r')
	| otherwise = return (Nothing, r)

gen' :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen' r u c gc = do
	cst <- remoteCost gc $
		if repoCheap r then nearlyCheapRemoteCost else expensiveRemoteCost
	(rsynctransport, rsyncurl) <- rsyncTransport r
	let rsyncopts = Remote.Rsync.genRsyncOpts c gc rsynctransport rsyncurl
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = \_ _ _ -> noCrypto
		, retrieveKeyFile = \_ _ _ _ -> noCrypto
		, retrieveKeyFileCheap = \_ _ -> return False
		, removeKey = remove this rsyncopts
		, hasKey = checkPresent this rsyncopts
		, hasKeyCheap = repoCheap r
		, whereisKey = Nothing
		, config = M.empty
		, localpath = localpathCalc r
		, repo = r
		, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
		, readonly = Git.repoIsHttp r
		, globallyAvailable = globallyAvailableCalc r
		, remotetype = remote
	}
	return $ Just $ encryptableRemote c
		(store this rsyncopts)
		(retrieve this rsyncopts)
		this

rsyncTransport :: Git.Repo -> Annex ([CommandParam], String)
rsyncTransport r
	| "ssh://" `isPrefixOf` loc = sshtransport $ break (== '/') $ drop (length "ssh://") loc
	| "//:" `isInfixOf` loc = othertransport
	| ":" `isInfixOf` loc = sshtransport $ separate (== ':') loc
	| otherwise = othertransport
  where
  	loc = Git.repoLocation r
	sshtransport (host, path) = do
		opts <- sshCachingOptions (host, Nothing) []
		return (rsyncShell $ Param "ssh" : opts, host ++ ":" ++ path)
	othertransport = return ([], loc)

noCrypto :: Annex a
noCrypto = error "cannot use gcrypt remote without encryption enabled"

unsupportedUrl :: Annex a
unsupportedUrl = error "using non-ssh remote repo url with gcrypt is not supported"

gCryptSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
gCryptSetup mu c = go $ M.lookup "gitrepo" c
  where
	remotename = fromJust (M.lookup "name" c)
  	go Nothing = error "Specify gitrepo="
	go (Just gitrepo) = do
		c' <- encryptionSetup c
		inRepo $ Git.Command.run 
			[ Params "remote add"
			, Param remotename
			, Param $ Git.GCrypt.urlPrefix ++ gitrepo
			]

		setGcryptEncryption c' remotename

		{- Run a git fetch and a push to the git repo in order to get
		 - its gcrypt-id set up, so that later git annex commands
		 - will use the remote as a ggcrypt remote. The fetch is
		 - needed if the repo already exists; the push is needed
		 - if the repo has not yet been initialized by gcrypt. -}
		void $ inRepo $ Git.Command.runBool
			[ Param "fetch"
			, Param remotename
			]
		void $ inRepo $ Git.Command.runBool
			[ Param "push"
			, Param remotename
			, Param $ show $ Annex.Branch.fullname
			]
		g <- inRepo Git.Config.reRead
		case Git.GCrypt.remoteRepoId g (Just remotename) of
			Nothing -> error "unable to determine gcrypt-id of remote"
			Just gcryptid -> do
				let u = genUUIDInNameSpace gCryptNameSpace gcryptid
				if Just u == mu || mu == Nothing
					then do
						-- Store gcrypt-id in local
						-- gcrypt repository, for later
						-- double-check.
						r <- inRepo $ Git.Construct.fromRemoteLocation gitrepo
						when (Git.repoIsLocalUnknown r) $ do
							r' <- liftIO $ Git.Config.read r
							liftIO $ Git.Command.run [Param "config", Param "core.gcrypt-id", Param gcryptid] r'
						gitConfigSpecialRemote u c' "gcrypt" "true"
						return (c', u)
					else error "uuid mismatch"

{- Configure gcrypt to use the same list of keyids that
 - were passed to initremote as its participants.
 - Also, configure it to use a signing key that is in the list of
 - participants, which gcrypt requires is the case, and may not be
 - depending on system configuration.
 -
 - (For shared encryption, gcrypt's default behavior is used.) -}
setGcryptEncryption :: RemoteConfig -> String -> Annex ()
setGcryptEncryption c remotename = do
	let participants = ConfigKey $ Git.GCrypt.remoteParticipantConfigKey remotename
	case extractCipher c of
		Nothing -> noCrypto
		Just (EncryptedCipher _ _ (KeyIds { keyIds = ks})) -> do
			setConfig participants (unwords ks)
			let signingkey = ConfigKey $ Git.GCrypt.remoteSigningKey remotename
			skeys <- M.keys <$> liftIO secretKeys
			case filter (`elem` ks) skeys of
				[] -> noop
				(k:_) -> setConfig signingkey k
		Just (SharedCipher _) ->
			unsetConfig participants

store :: Remote -> Remote.Rsync.RsyncOpts -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
store r rsyncopts (cipher, enck) k p
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $
		sendwith $ \meterupdate h -> do
			createDirectoryIfMissing True $ parentDir dest
			readBytes (meteredWriteFile meterupdate dest) h
			return True
	| Git.repoIsSsh (repo r) = Remote.Rsync.storeEncrypted rsyncopts gpgopts (cipher, enck) k p
	| otherwise = unsupportedUrl
  where
  	gpgopts = getGpgEncParams r
	dest = gCryptLocation r enck
  	sendwith a = metered (Just p) k $ \meterupdate ->
		Annex.Content.sendAnnex k noop $ \src ->
			liftIO $ catchBoolIO $
				encrypt gpgopts cipher (feedFile src) (a meterupdate)

retrieve :: Remote -> Remote.Rsync.RsyncOpts -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieve r rsyncopts (cipher, enck) k d p
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
		retrievewith $ L.readFile src
		return True
	| Git.repoIsSsh (repo r) = Remote.Rsync.retrieveEncrypted rsyncopts (cipher, enck) k d p
	| otherwise = unsupportedUrl
  where
	src = gCryptLocation r enck
	retrievewith a = metered (Just p) k $ \meterupdate -> liftIO $
		a >>= \b -> 
			decrypt cipher (feedBytes b)
				(readBytes $ meteredWriteFile meterupdate d)

remove :: Remote -> Remote.Rsync.RsyncOpts -> Key -> Annex Bool
remove r rsyncopts k
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
		liftIO $ removeDirectoryRecursive (parentDir dest)
		return True
	| Git.repoIsSsh (repo r) = Remote.Rsync.remove rsyncopts k
	| otherwise = unsupportedUrl
  where
	dest = gCryptLocation r k

checkPresent :: Remote -> Remote.Rsync.RsyncOpts -> Key -> Annex (Either String Bool)
checkPresent r rsyncopts k
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) unknown $
			liftIO $ catchDefaultIO unknown $
				Right <$> doesFileExist (gCryptLocation r k)
	| Git.repoIsSsh (repo r) = Remote.Rsync.checkPresent (repo r) rsyncopts k
	| otherwise = unsupportedUrl
  where
	unknown = Left $ "unable to check " ++ Git.repoDescribe (repo r) ++ show (repo r)

{- Annexed objects are stored directly under the top of the gcrypt repo
 - (not in annex/objects), and are hashed using lower-case directories for max
 - portability. -}
gCryptLocation :: Remote -> Key -> FilePath
gCryptLocation r key = Git.repoLocation (repo r) </> keyPath key hashDirLower
