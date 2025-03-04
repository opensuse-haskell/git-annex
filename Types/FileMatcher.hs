{- git-annex file matcher types
 -
 - Copyright 2013-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.FileMatcher where

import Types.UUID (UUID)
import Types.Key (Key)
import Types.Link (LinkType)
import Types.Mime
import Types.RepoSize (LiveUpdate)
import Utility.Matcher (Matcher, Token, MatchDesc)
import Utility.FileSize
import Utility.OsPath

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S

-- Information about a file and/or a key that can be matched on.
data MatchInfo
	= MatchingFile FileInfo
	| MatchingInfo ProvidedInfo
	| MatchingUserInfo UserProvidedInfo

data FileInfo = FileInfo
	{ contentFile :: OsPath
	-- ^ path to a file containing the content, for operations
	-- that examine it
	, matchFile :: OsPath
	-- ^ filepath to match on; may be relative to top of repo or cwd,
	-- depending on how globs in preferred content expressions
	-- are intended to be matched
	, matchKey :: Maybe Key
	-- ^ provided if a key is already known
	}

data ProvidedInfo = ProvidedInfo
	{ providedFilePath :: Maybe OsPath
	-- ^ filepath to match on, should not be accessed from disk.
	, providedKey :: Maybe Key
	, providedFileSize :: Maybe FileSize
	, providedMimeType :: Maybe MimeType
	, providedMimeEncoding :: Maybe MimeEncoding
	, providedLinkType :: Maybe LinkType
	}

keyMatchInfoWithoutContent :: Key -> OsPath -> MatchInfo
keyMatchInfoWithoutContent key file = MatchingInfo $ ProvidedInfo
	{ providedFilePath = Just file
	, providedKey = Just key
	, providedFileSize = Nothing
	, providedMimeType = Nothing
	, providedMimeEncoding = Nothing
	, providedLinkType = Nothing
	}

-- This is used when testing a matcher, with values to match against
-- provided by the user.
data UserProvidedInfo = UserProvidedInfo
	{ userProvidedFilePath :: UserInfo OsPath
	, userProvidedKey :: UserInfo Key
	, userProvidedFileSize :: UserInfo FileSize
	, userProvidedMimeType :: UserInfo MimeType
	, userProvidedMimeEncoding :: UserInfo MimeEncoding
	}

-- This may fail if the user did not provide the information.
type UserInfo a = Either (IO a) a

-- If the UserInfo is not available, accessing it may result in eg an
-- exception being thrown.
getUserInfo :: MonadIO m => UserInfo a -> m a
getUserInfo (Right i) = return i
getUserInfo (Left e) = liftIO e

newtype MatcherDesc = MatcherDesc String

type FileMatcherMap a = M.Map UUID (FileMatcher a)

type MkLimit a = String -> Either String (MatchFiles a)

type AssumeNotPresent = S.Set UUID

data MatchFiles a = MatchFiles 
	{ matchAction :: LiveUpdate -> AssumeNotPresent -> MatchInfo -> a Bool
	, matchNeedsFileName :: Bool
	-- ^ does the matchAction need a filename in order to match?
	, matchNeedsFileContent :: Bool
	-- ^ does the matchAction need the file content to be present in
	-- order to succeed?
	, matchNeedsKey :: Bool
	-- ^ does the matchAction look at information about the key?
	, matchNeedsLocationLog :: Bool
	-- ^ does the matchAction look at the location log?
	, matchNeedsLiveRepoSize :: Bool
	-- ^ does the matchAction need live repo size information?
	, matchNegationUnstable :: Bool
	-- ^ does negating the matchAction lead to unstable behavior?
	, matchDesc :: Maybe Bool -> MatchDesc
	-- ^ displayed to the user to describe whether it matched or not
	}

type FileMatcher a = (Matcher (MatchFiles a), MatcherDesc)

-- This is a matcher that can have tokens added to it while it's being
-- built, and once complete is compiled to an unchangeable matcher.
data ExpandableMatcher a
	= BuildingMatcher [Token (MatchFiles a)]
	| CompleteMatcher (Matcher (MatchFiles a))
