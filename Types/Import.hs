{- git-annex import types
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveGeneric #-}

module Types.Import where

import qualified Data.ByteString as S
import Data.Char
import Control.DeepSeq
import GHC.Generics

import Types.Export
import Utility.QuickCheck
import Utility.FileSystemEncoding

{- Location of content on a remote that can be imported. 
 - This is just an alias to ExportLocation, because both are referring to a
 - location on the remote. -}
type ImportLocation = ExportLocation

mkImportLocation :: RawFilePath -> ImportLocation
mkImportLocation = mkExportLocation

fromImportLocation :: ImportLocation -> RawFilePath
fromImportLocation = fromExportLocation

{- An identifier for content stored on a remote that has been imported into
 - the repository. It should be reasonably short since it is stored in the
 - git-annex branch.
 -
 - Since other things than git-annex can modify files on import remotes,
 - and git-annex then be used to import those modifications, the
 - ContentIdentifier needs to change when a file gets changed in such a
 - way. Device, inode, and size is one example of a good content
 - identifier. Or a hash if the remote's interface exposes hashes.
 -}
newtype ContentIdentifier = ContentIdentifier S.ByteString
	deriving (Eq, Ord, Show, Generic)

instance NFData ContentIdentifier

instance Arbitrary ContentIdentifier where
	-- Avoid non-ascii ContentIdentifiers because fully arbitrary
	-- strings may not be encoded using the filesystem
	-- encoding, which is normally applied to all input.
	arbitrary = ContentIdentifier . encodeBS
		<$> arbitrary `suchThat` all isAscii

{- List of files that can be imported from a remote, each with some added
 - information. -}
data ImportableContents info = ImportableContents
	{ importableContents :: [(ImportLocation, info)]
	, importableHistory :: [ImportableContents info]
	-- ^ Used by remotes that support importing historical versions of
	-- files that are stored in them. This is equivilant to a git
	-- commit history.
	--
	-- When retrieving a historical version of a file,
	-- old ImportLocations from importableHistory are not used;
	-- the content is no longer expected to be present at those
	-- locations. So, if a remote does not support Key/Value access,
	-- it should not populate the importableHistory.
	}
	deriving (Show, Generic)

instance NFData info => NFData (ImportableContents info)
