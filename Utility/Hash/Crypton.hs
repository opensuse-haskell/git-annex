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
	sha1_context,
	sha1s,
	sha2_224,
	sha2_224_context,
	sha2_256,
	sha2_256_context,
	sha2_384,
	sha2_384_context,
	sha2_512,
	sha2_512_context,
	sha3_224,
	sha3_224_context,
	sha3_256,
	sha3_256_context,
	sha3_384,
	sha3_384_context,
	sha3_512,
	sha3_512_context,
	skein256,
	skein256_context,
	skein512,
	skein512_context,
	blake2s_160,
	blake2s_160_context,
	blake2s_224,
	blake2s_224_context,
	blake2s_256,
	blake2s_256_context,
	blake2sp_224,
	blake2sp_224_context,
	blake2sp_256,
	blake2sp_256_context,
	blake2b_160,
	blake2b_160_context,
	blake2b_224,
	blake2b_224_context,
	blake2b_256,
	blake2b_256_context,
	blake2b_384,
	blake2b_384_context,
	blake2b_512,
	blake2b_512_context,
	blake2bp_512,
	blake2bp_512_context,
	md5,
	md5_context,
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IORef
import "crypton" Crypto.MAC.HMAC hiding (Context)
import "crypton" Crypto.Hash

import Utility.Hash.Incremental

sha1 :: L.ByteString -> Digest SHA1
sha1 = hashlazy

sha1_context :: Context SHA1
sha1_context = hashInit

sha1s :: S.ByteString -> Digest SHA1
sha1s = hash

sha2_224 :: L.ByteString -> Digest SHA224
sha2_224 = hashlazy

sha2_224_context :: Context SHA224
sha2_224_context = hashInit

sha2_256 :: L.ByteString -> Digest SHA256
sha2_256 = hashlazy

sha2_256_context :: Context SHA256
sha2_256_context = hashInit

sha2_384 :: L.ByteString -> Digest SHA384
sha2_384 = hashlazy

sha2_384_context :: Context SHA384
sha2_384_context = hashInit

sha2_512 :: L.ByteString -> Digest SHA512
sha2_512 = hashlazy

sha2_512_context :: Context SHA512
sha2_512_context = hashInit

sha3_224 :: L.ByteString -> Digest SHA3_224
sha3_224 = hashlazy

sha3_224_context :: Context SHA3_224
sha3_224_context = hashInit

sha3_256 :: L.ByteString -> Digest SHA3_256
sha3_256 = hashlazy

sha3_256_context :: Context SHA3_256
sha3_256_context = hashInit

sha3_384 :: L.ByteString -> Digest SHA3_384
sha3_384 = hashlazy

sha3_384_context :: Context SHA3_384
sha3_384_context = hashInit

sha3_512 :: L.ByteString -> Digest SHA3_512
sha3_512 = hashlazy

sha3_512_context :: Context SHA3_512
sha3_512_context = hashInit

skein256 :: L.ByteString -> Digest Skein256_256
skein256 = hashlazy

skein256_context :: Context Skein256_256
skein256_context = hashInit

skein512 :: L.ByteString -> Digest Skein512_512
skein512 = hashlazy

skein512_context :: Context Skein512_512
skein512_context = hashInit

blake2s_160 :: L.ByteString -> Digest Blake2s_160
blake2s_160 = hashlazy

blake2s_160_context :: Context Blake2s_160
blake2s_160_context = hashInit

blake2s_224 :: L.ByteString -> Digest Blake2s_224
blake2s_224 = hashlazy

blake2s_224_context :: Context Blake2s_224
blake2s_224_context = hashInit

blake2s_256 :: L.ByteString -> Digest Blake2s_256
blake2s_256 = hashlazy

blake2s_256_context :: Context Blake2s_256
blake2s_256_context = hashInit

blake2sp_224 :: L.ByteString -> Digest Blake2sp_224
blake2sp_224 = hashlazy

blake2sp_224_context :: Context Blake2sp_224
blake2sp_224_context = hashInit

blake2sp_256 :: L.ByteString -> Digest Blake2sp_256
blake2sp_256 = hashlazy

blake2sp_256_context :: Context Blake2sp_256
blake2sp_256_context = hashInit

blake2b_160 :: L.ByteString -> Digest Blake2b_160
blake2b_160 = hashlazy

blake2b_160_context :: Context Blake2b_160
blake2b_160_context = hashInit

blake2b_224 :: L.ByteString -> Digest Blake2b_224
blake2b_224 = hashlazy

blake2b_224_context :: Context Blake2b_224
blake2b_224_context = hashInit

blake2b_256 :: L.ByteString -> Digest Blake2b_256
blake2b_256 = hashlazy

blake2b_256_context :: Context Blake2b_256
blake2b_256_context = hashInit

blake2b_384 :: L.ByteString -> Digest Blake2b_384
blake2b_384 = hashlazy

blake2b_384_context :: Context Blake2b_384
blake2b_384_context = hashInit

blake2b_512 :: L.ByteString -> Digest Blake2b_512
blake2b_512 = hashlazy

blake2b_512_context :: Context Blake2b_512
blake2b_512_context = hashInit

blake2bp_512 :: L.ByteString -> Digest Blake2bp_512
blake2bp_512 = hashlazy

blake2bp_512_context :: Context Blake2bp_512
blake2bp_512_context = hashInit

md5 ::  L.ByteString -> Digest MD5
md5 = hashlazy

md5_context :: Context MD5
md5_context = hashInit

md5s ::  S.ByteString -> Digest MD5
md5s = hash

mkIncrementalHasher :: HashAlgorithm h => Context h -> String -> IO IncrementalHasher
mkIncrementalHasher ctx desc = do
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
				(Just (ctx', _)) -> do
					let digest = hashFinalize ctx'
					return $ Just $ show digest
				Nothing -> return Nothing
		, unableIncrementalHasher = writeIORef v Nothing
		, positionIncrementalHasher = readIORef v >>= \case
			Just (_, n) -> return (Just n)
			Nothing -> return Nothing
		, descIncrementalHasher = desc
		}

mkIncrementalVerifier :: HashAlgorithm h => Context h -> String -> (String -> Bool) -> IO IncrementalVerifier
mkIncrementalVerifier ctx desc samechecksum = do
	hasher <- mkIncrementalHasher ctx desc
	return $ incrementalHashVerifier hasher samechecksum
