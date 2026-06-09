{- Incremental hashing
 -
 - Copyright 2013-2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Hash.Incremental where

import qualified Data.ByteString as S

data IncrementalHasher = IncrementalHasher
	{ updateIncrementalHasher :: S.ByteString -> IO ()
	-- ^ Called repeatedly on each piece of the content.
	, finalizeIncrementalHasher :: IO (Maybe String)
	-- ^ Called once the full content has been sent, returns
	-- the hash. (Nothing if unableIncremental was called.)
	, unableIncrementalHasher :: IO ()
	-- ^ Call if the incremental hashing is unable to be done.
	, positionIncrementalHasher :: IO (Maybe Integer)
	-- ^ Returns the number of bytes that have been fed to this
	-- incremental hasher so far. (Nothing if unableIncremental was
	-- called.)
	, descIncrementalHasher :: String
	}

data IncrementalVerifier = IncrementalVerifier
	{ updateIncrementalVerifier :: S.ByteString -> IO ()
	, finalizeIncrementalVerifier :: IO (Maybe Bool)
	, unableIncrementalVerifier :: IO ()
	, positionIncrementalVerifier :: IO (Maybe Integer)
	, descIncrementalVerifier :: String
	}

incrementalHashVerifier :: IncrementalHasher -> (String -> Bool) -> IncrementalVerifier
incrementalHashVerifier hasher samechecksum = IncrementalVerifier
	{ updateIncrementalVerifier = updateIncrementalHasher hasher
	, finalizeIncrementalVerifier = 
		maybe Nothing (Just . samechecksum)
			<$> finalizeIncrementalHasher hasher
	, unableIncrementalVerifier = unableIncrementalHasher hasher
	, positionIncrementalVerifier = positionIncrementalHasher hasher
	, descIncrementalVerifier = descIncrementalHasher hasher
	}
