{- git repository handling 
 -
 - This is written to be completely independent of git-annex and should be
 - suitable for other uses.
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Git (
	Repo(..),
	Ref(..),
	fromRef,
	fromRef',
	Branch,
	Sha,
	Tag,
	repoIsUrl,
	repoIsSsh,
	repoIsHttp,
	repoIsLocal,
	repoIsLocalBare,
	repoIsLocalUnknown,
	repoDescribe,
	repoLocation,
	repoPath,
	repoWorkTree,
	localGitDir,
	attributes,
	attributesLocal,
	hookPath,
	assertLocal,
	adjustPath,
	relPath,
) where

import Network.URI (uriPath, uriScheme, unEscapeString)
#ifndef mingw32_HOST_OS
import System.Posix.Files
#endif

import Common
import Git.Types
import qualified Utility.OsString as OS
#ifndef mingw32_HOST_OS
import Utility.FileMode
#endif

{- User-visible description of a git repo. -}
repoDescribe :: Repo -> String
repoDescribe Repo { remoteName = Just name } = name
repoDescribe Repo { location = Url url } = show url
repoDescribe Repo { location = UnparseableUrl url } = url
repoDescribe Repo { location = Local { worktree = Just dir } } = fromOsPath dir
repoDescribe Repo { location = Local { gitdir = dir } } = fromOsPath dir
repoDescribe Repo { location = LocalUnknown dir } = fromOsPath dir
repoDescribe Repo { location = Unknown } = "UNKNOWN"

{- Location of the repo, either as a path or url. -}
repoLocation :: Repo -> String
repoLocation Repo { location = Url url } = show url
repoLocation Repo { location = UnparseableUrl url } = url
repoLocation Repo { location = Local { worktree = Just dir } } = fromOsPath dir
repoLocation Repo { location = Local { gitdir = dir } } = fromOsPath dir
repoLocation Repo { location = LocalUnknown dir } = fromOsPath dir
repoLocation Repo { location = Unknown } = giveup "unknown repoLocation"

{- Path to a repository. For non-bare, this is the worktree, for bare, 
 - it's the gitdir, and for URL repositories, is the path on the remote
 - host. -}
repoPath :: Repo -> OsPath
repoPath Repo { location = Url u } = toOsPath $ unEscapeString $ uriPath u
repoPath Repo { location = Local { worktree = Just d } } = d
repoPath Repo { location = Local { gitdir = d } } = d
repoPath Repo { location = LocalUnknown dir } = dir
repoPath Repo { location = Unknown } = giveup "unknown repoPath"
repoPath Repo { location = UnparseableUrl _u } = giveup "unknown repoPath"

repoWorkTree :: Repo -> Maybe OsPath
repoWorkTree Repo { location = Local { worktree = Just d } } = Just d
repoWorkTree _ = Nothing

{- Path to a local repository's .git directory. -}
localGitDir :: Repo -> OsPath
localGitDir Repo { location = Local { gitdir = d } } = d
localGitDir _ = giveup "unknown localGitDir"

repoIsUrl :: Repo -> Bool
repoIsUrl Repo { location = Url _ } = True
repoIsUrl Repo { location = UnparseableUrl _ } = True
repoIsUrl _ = False

repoIsSsh :: Repo -> Bool
repoIsSsh Repo { location = Url url } 
	| scheme == "ssh:" = True
	-- git treats these the same as ssh
	| scheme == "git+ssh:" = True
	| scheme == "ssh+git:" = True
	| otherwise = False
  where
	scheme = uriScheme url
repoIsSsh _ = False

repoIsHttp :: Repo -> Bool
repoIsHttp Repo { location = Url url } 
	| uriScheme url == "http:" = True
	| uriScheme url == "https:" = True
	| otherwise = False
repoIsHttp _ = False

repoIsLocal :: Repo -> Bool
repoIsLocal Repo { location = Local { } } = True
repoIsLocal _ = False

repoIsLocalBare :: Repo -> Bool
repoIsLocalBare Repo { location = Local { worktree = Nothing } } = True
repoIsLocalBare _ = False

repoIsLocalUnknown :: Repo -> Bool
repoIsLocalUnknown Repo { location = LocalUnknown { } } = True
repoIsLocalUnknown _ = False

assertLocal :: Repo -> a -> a
assertLocal repo action
	| repoIsUrl repo = giveup $ unwords
		[ "acting on non-local git repo"
		, repoDescribe repo
		, "not supported"
		]
	| otherwise = action

{- Path to a repository's gitattributes file. -}
attributes :: Repo -> OsPath
attributes repo
	| repoIsLocalBare repo = attributesLocal repo
	| otherwise = repoPath repo </> literalOsPath ".gitattributes"

attributesLocal :: Repo -> OsPath
attributesLocal repo = localGitDir repo </> literalOsPath "info" </> literalOsPath "attributes"

{- Path to a given hook script in a repository, only if the hook exists
 - and is executable. -}
hookPath :: String -> Repo -> IO (Maybe OsPath)
hookPath script repo = do
	let hook = localGitDir repo </> literalOsPath "hooks" </> toOsPath script
	ifM (catchBoolIO $ isexecutable hook)
		( return $ Just hook , return Nothing )
  where
#if mingw32_HOST_OS
	isexecutable f = doesFileExist f
#else
	isexecutable f = isExecutable . fileMode
		<$> getSymbolicLinkStatus (fromOsPath f)
#endif

{- Makes the path to a local Repo be relative to the cwd. -}
relPath :: Repo -> IO Repo
relPath = adjustPath torel
  where
	torel p = do
		p' <- relPathCwdToFile p
		return $ if OS.null p'
			then literalOsPath "."
			else p'

{- Adjusts the path to a local Repo using the provided function. -}
adjustPath :: (OsPath -> IO OsPath) -> Repo -> IO Repo
adjustPath f r@(Repo { location = l@(Local { gitdir = d, worktree = w }) }) = do
	d' <- f d
	w' <- maybe (pure Nothing) (Just <$$> f) w
	return $ r 
		{ location = l 
			{ gitdir = d'
			, worktree = w'
			}
		}
  where
adjustPath f r@(Repo { location = LocalUnknown d }) = do
	d' <- f d
	return $ r { location = LocalUnknown d' }
adjustPath _ r = pure r
