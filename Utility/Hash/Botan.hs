{- Convenience wrapper around botan's hashing.
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE BangPatterns, PackageImports #-}

module Utility.Hash.Botan (
	sha1,
	sha1s,
	sha2_224,
	sha2_256,
	sha2_384,
	sha2_512,
	sha3_224,
	sha3_256,
	sha3_384,
	sha3_512,
	skein512,
	blake2b_160,
        blake2b_224,
        blake2b_256,
        blake2b_384,
        blake2b_512,
	md5,
	md5s,
) where

import qualified Botan.Low.Hash as Botan
import Data.ByteString.Base16
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

import Utility.Hash.Types

sha1 :: L.ByteString -> Hash
sha1 = hashLazy Botan.SHA1

sha1s :: S.ByteString -> Hash
sha1s = hashStrict Botan.SHA1

sha2_224 :: L.ByteString -> Hash
sha2_224 = hashLazy Botan.SHA224

sha2_256 :: L.ByteString -> Hash
sha2_256 = hashLazy Botan.SHA256

sha2_384 :: L.ByteString -> Hash
sha2_384 = hashLazy Botan.SHA384

sha2_512 :: L.ByteString -> Hash
sha2_512 = hashLazy Botan.SHA512

sha3_224 :: L.ByteString -> Hash
sha3_224 = hashLazy (Botan.sha3 (224 :: Int))

sha3_256 :: L.ByteString -> Hash
sha3_256 = hashLazy (Botan.sha3 (256 :: Int))

sha3_384 :: L.ByteString -> Hash
sha3_384 = hashLazy (Botan.sha3 (384 :: Int))

sha3_512 :: L.ByteString -> Hash
sha3_512 = hashLazy (Botan.sha3 (512 :: Int))

skein512 :: L.ByteString -> Hash
skein512 = hashLazy Botan.Skein512
	
blake2b_160 :: L.ByteString -> Hash
blake2b_160 = hashLazy (Botan.blake2b (160 :: Int))

blake2b_224 :: L.ByteString -> Hash
blake2b_224 = hashLazy (Botan.blake2b (224 :: Int))

blake2b_256 :: L.ByteString -> Hash
blake2b_256 = hashLazy (Botan.blake2b (256 :: Int))

blake2b_384 :: L.ByteString -> Hash
blake2b_384 = hashLazy (Botan.blake2b (384 :: Int))

blake2b_512 :: L.ByteString -> Hash
blake2b_512 = hashLazy (Botan.blake2b (512 :: Int))

md5 :: L.ByteString -> Hash
md5 = hashLazy Botan.MD5

md5s :: S.ByteString -> Hash
md5s = hashStrict Botan.MD5

digestToHash :: Botan.HashDigest -> Hash
digestToHash = Hash . encodeBase16'

hashLazy :: Botan.HashName -> L.ByteString -> Hash
hashLazy h b = unsafePerformIO $ do
	hasher <- Botan.hashInit h
	forM_ (L.toChunks b) (Botan.hashUpdate hasher)
	digestToHash <$> Botan.hashFinal hasher

hashStrict :: Botan.HashName -> S.ByteString -> Hash
hashStrict h b = unsafePerformIO $ do
	hasher <- Botan.hashInit h
	Botan.hashUpdate hasher b
	digestToHash <$> Botan.hashFinal hasher

