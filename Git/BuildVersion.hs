{- git build version
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.BuildVersion where

import Git.Version
import qualified BuildInfo

{- Using the version it was configured for avoids running git to check its
 - version, at the cost that upgrading git won't be noticed.
 - This is only acceptable because it's rare that git's version influences
 - code's behavior. -}
buildVersion :: GitVersion
buildVersion = normalize BuildInfo.gitversion

older :: String -> Bool
older n = buildVersion < normalize n 
