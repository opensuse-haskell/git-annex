{- XXH3 convenience wrapper.
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Hash.XXH3 where

import Utility.Hash.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.Digest.XXHash.FFI as XXH3
import qualified Data.Hashable as Hashable
import Data.Word
import Data.Bits
import Data.ByteString.Builder

-- Due to use of Int, the xxhash-ffi library is currently only suitable
-- for use on 64 bit (or higher) systems. Trying to use it on
-- 32 bit systems will produce non-canonical values.
-- https://github.com/haskell-haskey/xxhash-ffi/issues/6
xxH3Supported :: Bool
xxH3Supported = finiteBitSize (0 :: Int) >= 64

xxh3 :: L.ByteString -> HashDigest
xxh3 = convcanonical . Hashable.hashWithSalt 0 . XXH3.XXH3
  where
	-- Convert to XXH3 canonical representation.
	-- This is unfortunately not exposed by xxhash-ffi so has to
	-- be re-implemented here.
	-- See https://github.com/haskell-haskey/xxhash-ffi/issues/8
	convcanonical = HashDigest 
		. L.toStrict . toLazyByteString 
		. word64BE . fromint
	fromint :: Int -> Word64
	fromint = fromIntegral
	
-- The haskell library does not support incremental verification
-- (without going too low-level to be appropriate here).
-- https://github.com/haskell-haskey/xxhash-ffi/issues/7
xxh3Incremental :: Maybe a
xxh3Incremental = Nothing
