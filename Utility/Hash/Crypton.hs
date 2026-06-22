{- Convenience wrapper around crypton's hashing.
 -
 - Copyright 2013-2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE BangPatterns, PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Utility.Hash.Crypton (
	sha1,
	sha1_hasher,
	sha1s,
	sha2_224,
	sha2_224_hasher,
	sha2_256,
	sha2_256_hasher,
	sha2_384,
	sha2_384_hasher,
	sha2_512,
	sha2_512_hasher,
	sha3_224,
	sha3_224_hasher,
	sha3_256,
	sha3_256_hasher,
	sha3_384,
	sha3_384_hasher,
	sha3_512,
	sha3_512_hasher,
	skein256,
	skein256_hasher,
	skein512,
	skein512_hasher,
	blake2s_160,
	blake2s_160_hasher,
	blake2s_224,
	blake2s_224_hasher,
	blake2s_256,
	blake2s_256_hasher,
	blake2sp_224,
	blake2sp_224_hasher,
	blake2sp_256,
	blake2sp_256_hasher,
	blake2b_160,
	blake2b_160_hasher,
	blake2b_224,
	blake2b_224_hasher,
	blake2b_256,
	blake2b_256_hasher,
	blake2b_384,
	blake2b_384_hasher,
	blake2b_512,
	blake2b_512_hasher,
	blake2bp_512,
	blake2bp_512_hasher,
	md5,
	md5_hasher,
	md5s,
	hashUpdate,
	hashFinalize,
	Digest,
	HashAlgorithm,
	Context,
	mkIncrementalHasher,
	mkIncrementalVerifier,
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import qualified Data.ByteArray as BA
import "crypton" Crypto.Hash

import Utility.Hash.Types
import Utility.Hash.Incremental

sha1 :: L.ByteString -> HashDigest
sha1 = hashDigest . (hashlazy :: HashLazy SHA1)

sha1_hasher :: IO IncrementalHasher
sha1_hasher = mkIncrementalHasher (hashInit :: Context SHA1)

sha1s :: S.ByteString -> HashDigest
sha1s = hashDigest . (hash :: HashStrict SHA1)

sha2_224 :: L.ByteString -> HashDigest
sha2_224 = hashDigest . (hashlazy :: HashLazy SHA224)

sha2_224_hasher :: IO IncrementalHasher
sha2_224_hasher = mkIncrementalHasher (hashInit :: Context SHA224)

sha2_256 :: L.ByteString -> HashDigest
sha2_256 = hashDigest . (hashlazy :: HashLazy SHA256)

sha2_256_hasher :: IO IncrementalHasher
sha2_256_hasher = mkIncrementalHasher (hashInit :: Context SHA256)

sha2_384 :: L.ByteString -> HashDigest
sha2_384 = hashDigest . (hashlazy :: HashLazy SHA384)

sha2_384_hasher :: IO IncrementalHasher
sha2_384_hasher = mkIncrementalHasher (hashInit :: Context SHA384)

sha2_512 :: L.ByteString -> HashDigest
sha2_512 = hashDigest . (hashlazy :: HashLazy SHA512)

sha2_512_hasher :: IO IncrementalHasher
sha2_512_hasher = mkIncrementalHasher (hashInit :: Context SHA512)

sha3_224 :: L.ByteString -> HashDigest
sha3_224 = hashDigest . (hashlazy :: HashLazy SHA3_224)

sha3_224_hasher :: IO IncrementalHasher
sha3_224_hasher = mkIncrementalHasher (hashInit :: Context SHA3_224)

sha3_256 :: L.ByteString -> HashDigest
sha3_256 = hashDigest . (hashlazy :: HashLazy SHA3_256)

sha3_256_hasher :: IO IncrementalHasher
sha3_256_hasher = mkIncrementalHasher (hashInit :: Context SHA3_256)

sha3_384 :: L.ByteString -> HashDigest
sha3_384 = hashDigest . (hashlazy :: HashLazy SHA3_384)

sha3_384_hasher :: IO IncrementalHasher
sha3_384_hasher = mkIncrementalHasher (hashInit :: Context SHA3_384)

sha3_512 :: L.ByteString -> HashDigest
sha3_512 = hashDigest . (hashlazy :: HashLazy SHA3_512)

sha3_512_hasher :: IO IncrementalHasher
sha3_512_hasher = mkIncrementalHasher (hashInit :: Context SHA3_512)

skein256 :: L.ByteString -> HashDigest
skein256 = hashDigest . (hashlazy :: HashLazy Skein256_256)

skein256_hasher :: IO IncrementalHasher
skein256_hasher = mkIncrementalHasher (hashInit :: Context Skein256_256)

skein512 :: L.ByteString -> HashDigest
skein512 = hashDigest . (hashlazy :: HashLazy Skein512_512)

skein512_hasher :: IO IncrementalHasher
skein512_hasher = mkIncrementalHasher (hashInit :: Context Skein512_512)

blake2s_160 :: L.ByteString -> HashDigest
blake2s_160 = hashDigest . (hashlazy :: HashLazy Blake2s_160)

blake2s_160_hasher :: IO IncrementalHasher
blake2s_160_hasher = mkIncrementalHasher (hashInit :: Context Blake2s_160)

blake2s_224 :: L.ByteString -> HashDigest
blake2s_224 = hashDigest . (hashlazy :: HashLazy Blake2s_224)

blake2s_224_hasher :: IO IncrementalHasher
blake2s_224_hasher = mkIncrementalHasher (hashInit :: Context Blake2s_224)

blake2s_256 :: L.ByteString -> HashDigest
blake2s_256 = hashDigest . (hashlazy :: HashLazy Blake2s_256)

blake2s_256_hasher :: IO IncrementalHasher
blake2s_256_hasher = mkIncrementalHasher (hashInit :: Context Blake2s_256)

blake2sp_224 :: L.ByteString -> HashDigest
blake2sp_224 = hashDigest . (hashlazy :: HashLazy Blake2sp_224)

blake2sp_224_hasher :: IO IncrementalHasher
blake2sp_224_hasher = mkIncrementalHasher (hashInit :: Context Blake2sp_224)

blake2sp_256 :: L.ByteString -> HashDigest
blake2sp_256 = hashDigest . (hashlazy :: HashLazy Blake2sp_256)

blake2sp_256_hasher :: IO IncrementalHasher
blake2sp_256_hasher = mkIncrementalHasher (hashInit :: Context Blake2sp_256)

blake2b_160 :: L.ByteString -> HashDigest
blake2b_160 = hashDigest . (hashlazy :: HashLazy Blake2b_160)

blake2b_160_hasher :: IO IncrementalHasher
blake2b_160_hasher = mkIncrementalHasher (hashInit :: Context Blake2b_160)

blake2b_224 :: L.ByteString -> HashDigest
blake2b_224 = hashDigest . (hashlazy :: HashLazy Blake2b_224)

blake2b_224_hasher :: IO IncrementalHasher
blake2b_224_hasher = mkIncrementalHasher (hashInit :: Context Blake2b_224)

blake2b_256 :: L.ByteString -> HashDigest
blake2b_256 = hashDigest . (hashlazy :: HashLazy Blake2b_256)

blake2b_256_hasher :: IO IncrementalHasher
blake2b_256_hasher = mkIncrementalHasher (hashInit :: Context Blake2b_256)

blake2b_384 :: L.ByteString -> HashDigest
blake2b_384 = hashDigest . (hashlazy :: HashLazy Blake2b_384)

blake2b_384_hasher :: IO IncrementalHasher
blake2b_384_hasher = mkIncrementalHasher (hashInit :: Context Blake2b_384)

blake2b_512 :: L.ByteString -> HashDigest
blake2b_512 = hashDigest . (hashlazy :: HashLazy Blake2b_512)

blake2b_512_hasher :: IO IncrementalHasher
blake2b_512_hasher = mkIncrementalHasher (hashInit :: Context Blake2b_512)

blake2bp_512 :: L.ByteString -> HashDigest
blake2bp_512 = hashDigest . (hashlazy :: HashLazy Blake2bp_512)

blake2bp_512_hasher :: IO IncrementalHasher
blake2bp_512_hasher = mkIncrementalHasher (hashInit :: Context Blake2bp_512)

md5 ::  L.ByteString -> HashDigest
md5 = hashDigest . (hashlazy :: HashLazy MD5)

md5_hasher :: IO IncrementalHasher
md5_hasher = mkIncrementalHasher (hashInit :: Context MD5)

md5s ::  S.ByteString -> HashDigest
md5s = hashDigest . (hash :: HashStrict MD5)

type HashStrict t = S.ByteString -> Digest t

type HashLazy t = L.ByteString -> Digest t

hashDigest :: Digest a -> HashDigest
hashDigest = HashDigest . BA.convert

mkIncrementalHasher :: HashAlgorithm h => Context h -> IO IncrementalHasher
mkIncrementalHasher ctx = do
	v <- newIORef (Just (ctx, 0))
	return $ IncrementalHasher
		{ updateIncrementalHasher = \b ->
			modifyIORef' v $ \case
				(Just (ctx', n)) -> 
					let !ctx'' = hashUpdate ctx' b
					    !n' = n + fromIntegral (S.length b)
					in (Just (ctx'', n'))
				Nothing -> Nothing
		, finalizeIncrementalHasher =
			readIORef v >>= \case
				(Just (ctx', _)) -> return $ Just $
					hashDigest $ hashFinalize ctx'
				Nothing -> return Nothing
		, unableIncrementalHasher = writeIORef v Nothing
		, positionIncrementalHasher = readIORef v >>= \case
			Just (_, n) -> return (Just n)
			Nothing -> return Nothing
		}
