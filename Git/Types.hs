{- git data types
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Git.Types where

import Utility.SafeCommand
import Utility.FileSystemEncoding
import Utility.OsPath

import Network.URI
import Data.String
import Data.Default
import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.List.NonEmpty as NE
import System.Posix.Types
import qualified Data.Semigroup as Sem
import Prelude

{- Support repositories on local disk, and repositories accessed via an URL.
 -
 - Repos on local disk have a git directory, and unless bare, a worktree.
 -
 - A local repo may not have had its config read yet, in which case all
 - that's known about it is its path.
 -
 - Finally, an Unknown repository may be known to exist, but nothing
 - else known about it.
 -}
data RepoLocation
	= Local { gitdir :: OsPath, worktree :: Maybe OsPath }
	| LocalUnknown OsPath
	| Url URI
	| UnparseableUrl String
	| Unknown
	deriving (Show, Eq, Ord)

data Repo = Repo
	{ location :: RepoLocation
	, config :: RepoConfig
	-- a given git config key can actually have multiple values
	, fullconfig :: RepoFullConfig
	-- remoteName holds the name used for this repo in some other
	-- repo's list of remotes, when this repo is such a remote
	, remoteName :: Maybe RemoteName
	-- alternate environment to use when running git commands
	, gitEnv :: Maybe [(String, String)]
	, gitEnvOverridesGitDir :: Bool
	-- global options to pass to git when running git commands
	, gitGlobalOpts :: [CommandParam]
	-- True only when --git-dir or GIT_DIR was used
	, gitDirSpecifiedExplicitly :: Bool
	-- Use when the path to the repository was specified explicitly,
	-- eg in a git remote, and so it's safe to set 
	-- -c safe.directory=* and -c safe.bareRepository=all 
	-- when using this repository.
	, repoPathSpecifiedExplicitly :: Bool
	-- When a Repo is a linked git worktree, this is the path
	-- from its gitdir to the git directory of the main worktree.
	, mainWorkTreePath :: Maybe OsPath
	} deriving (Show, Eq, Ord)
	
type RepoConfig = M.Map ConfigKey ConfigValue

type RepoFullConfig = M.Map ConfigKey (NE.NonEmpty ConfigValue)

newtype ConfigKey = ConfigKey S.ByteString
	deriving (Ord, Eq)

data ConfigValue
	= ConfigValue S.ByteString
	| NoConfigValue
	-- ^ git treats a setting with no value as different than a setting
	-- with an empty value
	deriving (Ord, Eq)

instance Sem.Semigroup ConfigValue where
	ConfigValue a <> ConfigValue b = ConfigValue (a <> b)
	a <> NoConfigValue = a
	NoConfigValue <> b = b

instance Monoid ConfigValue where
	mempty = ConfigValue mempty

instance Default ConfigValue where
	def = ConfigValue mempty

fromConfigKey :: ConfigKey -> String
fromConfigKey (ConfigKey s) = decodeBS s

fromConfigKey' :: ConfigKey -> S.ByteString
fromConfigKey' (ConfigKey s) = s

instance Show ConfigKey where
	show = fromConfigKey

class FromConfigValue a where
	fromConfigValue :: ConfigValue -> a

instance FromConfigValue S.ByteString where
	fromConfigValue (ConfigValue s) = s
	fromConfigValue NoConfigValue = mempty

instance FromConfigValue String where
	fromConfigValue = decodeBS . fromConfigValue

#ifdef WITH_OSPATH
instance FromConfigValue OsPath where
	fromConfigValue v = toOsPath (fromConfigValue v :: S.ByteString)
#endif

instance Show ConfigValue where
	show = fromConfigValue

instance IsString ConfigKey where
	fromString = ConfigKey . encodeBS

instance IsString ConfigValue where
	fromString = ConfigValue . encodeBS

type RemoteName = String

{- A git ref. Can be a sha1, or a branch or tag name. -}
newtype Ref = Ref S.ByteString
	deriving (Eq, Ord, Read, Show)

fromRef :: Ref -> String
fromRef = decodeBS . fromRef'

fromRef' :: Ref -> S.ByteString
fromRef' (Ref s) = s

{- Aliases for Ref. -}
type Branch = Ref
type Sha = Ref
type Tag = Ref

{- A date in the format described in gitrevisions. Includes the
 - braces, eg, "{yesterday}" -}
newtype RefDate = RefDate String

{- Types of objects that can be stored in git. -}
data ObjectType = BlobObject | CommitObject | TreeObject
	deriving (Show, Eq)

readObjectType :: S.ByteString -> Maybe ObjectType
readObjectType "blob" = Just BlobObject
readObjectType "commit" = Just CommitObject
readObjectType "tree" = Just TreeObject
readObjectType _ = Nothing

fmtObjectType :: ObjectType -> S.ByteString
fmtObjectType BlobObject = "blob"
fmtObjectType CommitObject = "commit"
fmtObjectType TreeObject = "tree"

{- Types of items in a tree. -}
data TreeItemType
	= TreeFile
	| TreeExecutable
	| TreeSymlink
	| TreeSubmodule
	| TreeSubtree
	deriving (Eq, Show)

{- Git uses magic numbers to denote the type of a tree item. -}
readTreeItemType :: S.ByteString -> Maybe TreeItemType
readTreeItemType "100644" = Just TreeFile
readTreeItemType "100755" = Just TreeExecutable
readTreeItemType "120000" = Just TreeSymlink
readTreeItemType "160000" = Just TreeSubmodule
readTreeItemType "040000" = Just TreeSubtree
readTreeItemType _ = Nothing

fmtTreeItemType :: TreeItemType -> S.ByteString
fmtTreeItemType TreeFile = "100644"
fmtTreeItemType TreeExecutable = "100755"
fmtTreeItemType TreeSymlink = "120000"
fmtTreeItemType TreeSubmodule = "160000"
fmtTreeItemType TreeSubtree = "040000"

toTreeItemType :: FileMode -> Maybe TreeItemType
toTreeItemType 0o100644 = Just TreeFile
toTreeItemType 0o100755 = Just TreeExecutable
toTreeItemType 0o120000 = Just TreeSymlink
toTreeItemType 0o160000 = Just TreeSubmodule
toTreeItemType 0o040000 = Just TreeSubtree
toTreeItemType _ = Nothing

fromTreeItemType :: TreeItemType -> FileMode
fromTreeItemType TreeFile = 0o100644
fromTreeItemType TreeExecutable = 0o100755
fromTreeItemType TreeSymlink = 0o120000
fromTreeItemType TreeSubmodule = 0o160000
fromTreeItemType TreeSubtree = 0o040000

data Commit = Commit
	{ commitTree :: Sha
	, commitParent :: [Sha]
	, commitAuthorMetaData :: CommitMetaData
	, commitCommitterMetaData :: CommitMetaData
	, commitMessage :: String
	}
	deriving (Show)

data CommitMetaData = CommitMetaData
	{ commitName :: Maybe String
	, commitEmail :: Maybe String
	, commitDate :: Maybe String -- In raw git form, "epoch -tzoffset"
	}
	deriving (Show)
