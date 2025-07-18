{- Construction of Git Repo objects
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Git.Construct (
	fromCwd,
	fromAbsPath,
	fromPath,
	fromUrl,
	fromUnknown,
	localToUrl,
	remoteNamed,
	remoteNamedFromKey,
	fromRemotes,
	fromRemoteUrlRemotes,
	fromRemoteLocation,
	repoAbsPath,
	checkForRepo,
	newFrom,
	adjustGitDirFile,
	isBareRepo,
) where

#ifndef mingw32_HOST_OS
import System.Posix.User
#endif
import qualified Data.Map as M
import Network.URI

import Common
import Git.Types
import Git
import Git.Remote
import Git.FilePath
import qualified Git.Url as Url
import Utility.UserInfo
import Utility.Url.Parse
import qualified Utility.OsString as OS

{- Finds the git repository used for the cwd, which may be in a parent
 - directory. -}
fromCwd :: IO (Maybe Repo)
fromCwd = getCurrentDirectory >>= seekUp
  where
	seekUp dir = do
		r <- checkForRepo dir
		case r of
			Nothing -> case upFrom dir of
				Nothing -> return Nothing
				Just d -> seekUp d
			Just loc -> pure $ Just $ newFrom loc

{- Local Repo constructor, accepts a relative or absolute path. -}
fromPath :: OsPath -> IO Repo
fromPath dir
	-- When dir == "foo/.git", git looks for "foo/.git/.git",
	-- and failing that, uses "foo" as the repository.
	| (pathSeparator `OS.cons` dotgit) `OS.isSuffixOf` canondir =
		ifM (doesDirectoryExist $ dir </> dotgit)
			( ret dir
			, ret (takeDirectory canondir)
			)
	| otherwise = ifM (doesDirectoryExist dir)
		( checkGitDirFile dir >>= maybe (ret dir) (pure . newFrom)
		-- git falls back to dir.git when dir doesn't
		-- exist, as long as dir didn't end with a
		-- path separator
		, if dir == canondir
			then ret (dir <> dotgit)
			else ret dir
		)
  where
	dotgit = literalOsPath ".git"
	ret = pure . newFrom . LocalUnknown
	canondir = dropTrailingPathSeparator dir

{- Local Repo constructor, requires an absolute path to the repo be
 - specified. -}
fromAbsPath :: OsPath -> IO Repo
fromAbsPath dir
	| absoluteGitPath dir = fromPath dir
	| otherwise =
		giveup $ "internal error, " ++ show dir ++ " is not absolute"

{- Construct a Repo for a remote's url.
 -
 - Git is somewhat forgiving about urls to repositories, allowing
 - eg spaces that are not normally allowed unescaped in urls. Such
 - characters get escaped.
 -
 - This will always succeed, even if the url cannot be parsed
 - or is invalid, because git can also function despite remotes having
 - such urls, only failing if such a remote is used.
 -}
fromUrl :: Bool -> String -> IO Repo
fromUrl fileurlislocal url
	| not (isURI url) = fromUrl' fileurlislocal $
		escapeURIString isUnescapedInURI url
	| otherwise = fromUrl' fileurlislocal url

fromUrl' :: Bool -> String -> IO Repo
fromUrl' fileurlislocal url
	| "file://" `isPrefixOf` url && fileurlislocal = case parseURIPortable url of
		Just u -> fromAbsPath $ toOsPath $ unEscapeString $ uriPath u
		Nothing -> pure $ newFrom $ UnparseableUrl url
	| otherwise = case parseURIPortable url of
		Just u -> pure $ newFrom $ Url u
		Nothing -> pure $ newFrom $ UnparseableUrl url

{- Creates a repo that has an unknown location. -}
fromUnknown :: Repo
fromUnknown = newFrom Unknown

{- Converts a local Repo into a remote repo, using the reference repo
 - which is assumed to be on the same host. -}
localToUrl :: Repo -> Repo -> Repo
localToUrl reference r
	| not $ repoIsUrl reference = error "internal error; reference repo not url"
	| repoIsUrl r = r
	| otherwise = case (Url.authority reference, Url.scheme reference) of
		(Just auth, Just s) -> 
			let referencepath = fromMaybe "" $ Url.path reference
			    absurl = concat
				[ s
				, "//"
				, auth
				, fromOsPath $ simplifyPath $
					toOsPath referencepath </> repoPath r
				]
			in r { location = Url $ fromJust $ parseURIPortable absurl }
		_ -> r

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
fromRemotes :: Repo -> IO [Repo]
fromRemotes = fromRemotes' fromRemoteLocation

fromRemotes' :: (String -> Bool -> Repo -> IO Repo) -> Repo -> IO [Repo]
fromRemotes' fromremotelocation repo = catMaybes <$> mapM construct remotepairs
  where
	filterconfig f = filter f $ M.toList $ config repo
	filterkeys f = filterconfig (\(k,_) -> f k)
	remotepairs = filterkeys isRemoteUrlKey
	construct (k,v) = remoteNamedFromKey k $
		fromremotelocation (fromConfigValue v) False repo

{- Calculates a list of a remote repo's configured remotes, by parsing its
 - config. Unlike fromRemotes, this does not do any local path checking. 
 - The remote repo must have an url path. -}
fromRemoteUrlRemotes :: Repo -> IO [Repo]
fromRemoteUrlRemotes = fromRemotes' go
  where
	go s knownurl repo = 
		case parseRemoteLocation s knownurl repo of
			RemotePath p -> pure $ localToUrl repo $ 
				newFrom $ LocalUnknown $ toOsPath p
			RemoteUrl u -> fromUrl False u

{- Sets the name of a remote when constructing the Repo to represent it. -}
remoteNamed :: String -> IO Repo -> IO Repo
remoteNamed n constructor = do
	r <- constructor
	return $ r { remoteName = Just n }

{- Sets the name of a remote based on the git config key, such as
 - "remote.foo.url". -}
remoteNamedFromKey :: ConfigKey -> IO Repo -> IO (Maybe Repo)
remoteNamedFromKey k r = case remoteKeyToRemoteName k of
	Nothing -> pure Nothing
	Just n -> Just <$> remoteNamed n r

{- Constructs a new Repo for one of a Repo's remotes using a given
 - location (ie, an url). 
 -
 - knownurl can be true if the location is known to be an url. This allows
 - urls that don't parse as urls to be used, returning UnparseableUrl.
 - If knownurl is false, the location may still be an url, if it parses as
 - one.
 -}
fromRemoteLocation :: String -> Bool -> Repo -> IO Repo
fromRemoteLocation s knownurl repo = gen $ parseRemoteLocation s knownurl repo
  where
	gen (RemotePath p) = fromRemotePath p repo
	gen (RemoteUrl u) = fromUrl True u

{- Constructs a Repo from the path specified in the git remotes of
 - another Repo. -}
fromRemotePath :: FilePath -> Repo -> IO Repo
fromRemotePath dir repo = do
	dir' <- expandTilde dir
	fromPath $ repoPath repo </> dir'

{- Git remotes can have a directory that is specified relative
 - to the user's home directory, or that contains tilde expansions.
 - This converts such a directory to an absolute path.
 - Note that it has to run on the system where the remote is.
 -}
repoAbsPath :: OsPath -> IO OsPath
repoAbsPath d = do
	d' <- expandTilde (fromOsPath d)
	h <- myHomeDir
	return $ toOsPath h </> d'

expandTilde :: FilePath -> IO OsPath
#ifdef mingw32_HOST_OS
expandTilde = return . toOsPath
#else
expandTilde p = expandt True p
	-- If unable to expand a tilde, eg due to a user not existing,
	-- use the path as given.
	`catchNonAsync` (const (return (toOsPath p)))
  where
	expandt _ [] = return $ literalOsPath ""
	expandt _ ('/':cs) = do
		v <- expandt True cs
		return $ literalOsPath "/" <> v
	expandt True ('~':'/':cs) = do
		h <- myHomeDir
		return $ toOsPath h </> toOsPath cs
	expandt True "~" = toOsPath <$> myHomeDir
	expandt True ('~':cs) = do
		let (name, rest) = findname "" cs
		u <- getUserEntryForName name
		return $ toOsPath (homeDirectory u) </> toOsPath rest
	expandt _ (c:cs) = do
		v <- expandt False cs
		return $ toOsPath [c] <> v
	findname n [] = (n, "")
	findname n (c:cs)
		| c == '/' = (n, cs)
		| otherwise = findname (n++[c]) cs
#endif

{- Checks if a git repository exists in a directory. Does not find
 - git repositories in parent directories. -}
checkForRepo :: OsPath -> IO (Maybe RepoLocation)
checkForRepo dir = 
	check isRepo $
		check (checkGitDirFile dir) $
			check (checkdir (isBareRepo dir)) $
				return Nothing
  where
	check test cont = maybe cont (return . Just) =<< test
	checkdir c = ifM c
		( return $ Just $ LocalUnknown dir
		, return Nothing
		)
	isRepo = checkdir $ 
		doesFileExist (dir </> literalOsPath ".git" </> literalOsPath "config")
			<||>
		-- A git-worktree lacks .git/config, but has .git/gitdir.
		-- (Normally the .git is a file, not a symlink, but it can
		-- be converted to a symlink and git will still work;
		-- this handles that case.)
		doesFileExist (dir </>  literalOsPath ".git" </> literalOsPath "gitdir")

isBareRepo :: OsPath -> IO Bool
isBareRepo dir = doesFileExist (dir </> literalOsPath "config")
	<&&> doesDirectoryExist (dir </> literalOsPath "objects")

-- Check for a .git file.
checkGitDirFile :: OsPath -> IO (Maybe RepoLocation)
checkGitDirFile dir = adjustGitDirFile' $ Local 
	{ gitdir = dir </> literalOsPath ".git"
	, worktree = Just dir
	}

-- git-submodule, git-worktree, and --separate-git-dir
-- make .git be a file pointing to the real git directory.
-- Detect that, and return a RepoLocation with gitdir pointing 
-- to the real git directory.
adjustGitDirFile :: RepoLocation -> IO RepoLocation
adjustGitDirFile loc = fromMaybe loc <$> adjustGitDirFile' loc

adjustGitDirFile' :: RepoLocation -> IO (Maybe RepoLocation)
adjustGitDirFile' loc@(Local {}) = do
	let gd = gitdir loc
	c <- firstLine <$> catchDefaultIO "" (readFile (fromOsPath gd))
	if gitdirprefix `isPrefixOf` c
		then do
			top <- takeDirectory <$> absPath gd
			return $ Just $ loc
				{ gitdir = absPathFrom top $ 
					toOsPath $ drop (length gitdirprefix) c
				}
		else return Nothing
 where
	gitdirprefix = "gitdir: "
adjustGitDirFile' _ = error "internal"

newFrom :: RepoLocation -> Repo
newFrom l = Repo
	{ location = l
	, config = M.empty
	, fullconfig = M.empty
	, remoteName = Nothing
	, gitEnv = Nothing
	, gitEnvOverridesGitDir = False
	, gitGlobalOpts = []
	, gitDirSpecifiedExplicitly = False
	, repoPathSpecifiedExplicitly = False
	, mainWorkTreePath = Nothing
	}

