{- git-annex key backend data type
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Backend where

import Types.Key
import Types.KeySource
import Utility.Metered
import Utility.OsPath
import Utility.FileSystemEncoding
import Utility.Hash (IncrementalVerifier)

data BackendA a = Backend
	{ backendVariety :: KeyVariety
	, genKey :: Maybe (KeySource -> MeterUpdate -> a Key)
	-- Verifies the content of a key, stored in a file, using a hash.
	-- This does not need to be cryptographically secure.
	, verifyKeyContent :: Maybe (Key -> OsPath -> a Bool)
	-- Incrementally verifies the content of a key, using the same
	-- hash as verifyKeyContent, but with the content provided
	-- incrementally a piece at a time, until finalized.
	, verifyKeyContentIncrementally :: Maybe (Key -> a IncrementalVerifier)
	-- Checks if a key can be upgraded to a better form.
	, canUpgradeKey :: Maybe (Key -> Bool)
	-- Checks if there is a fast way to migrate a key to a different
	-- backend (ie, without re-hashing).
	-- The Bool is true if the content of the key has been verified to
	-- be inAnnex.
	, fastMigrate :: Maybe (Key -> BackendA a -> AssociatedFile -> Bool -> a (Maybe Key))
	-- Checks if a key is known (or assumed) to always refer to the
	-- same data.
	, isStableKey :: Key -> Bool
	-- Are all keys using this backend verified using a cryptographically
	-- secure hash?
	, isCryptographicallySecure :: Bool
	-- Checks if a key is verified using a cryptographically secure hash.
	, isCryptographicallySecureKey :: Key -> a Bool
	}

instance Show (BackendA a) where
	show backend = "Backend { name =\"" ++ decodeBS (formatKeyVariety (backendVariety backend)) ++ "\" }"

instance Eq (BackendA a) where
	a == b = backendVariety a == backendVariety b
