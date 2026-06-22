{- Convenience wrapper around botan's hashing.
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE BangPatterns #-}

module Utility.Hash.Botan (
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
	skein512,
	skein512_hasher,
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
	md5,
	md5_hasher,
	md5s,
) where

import qualified Botan.Low.Hash as Botan
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.IORef

import Utility.Hash.Types
import Utility.Hash.Incremental

sha1 :: L.ByteString -> HashDigest
sha1 = hashLazy Botan.SHA1

sha1_hasher :: IO IncrementalHasher
sha1_hasher = mkIncrementalHasher Botan.SHA1

sha1s :: S.ByteString -> HashDigest
sha1s = hashStrict Botan.SHA1

sha2_224 :: L.ByteString -> HashDigest
sha2_224 = hashLazy Botan.SHA224

sha2_224_hasher :: IO IncrementalHasher
sha2_224_hasher = mkIncrementalHasher Botan.SHA224

sha2_256 :: L.ByteString -> HashDigest
sha2_256 = hashLazy Botan.SHA256

sha2_256_hasher :: IO IncrementalHasher
sha2_256_hasher = mkIncrementalHasher Botan.SHA256

sha2_384 :: L.ByteString -> HashDigest
sha2_384 = hashLazy Botan.SHA384

sha2_384_hasher :: IO IncrementalHasher
sha2_384_hasher = mkIncrementalHasher Botan.SHA384

sha2_512 :: L.ByteString -> HashDigest
sha2_512 = hashLazy Botan.SHA512

sha2_512_hasher :: IO IncrementalHasher
sha2_512_hasher = mkIncrementalHasher Botan.SHA512

sha3_224 :: L.ByteString -> HashDigest
sha3_224 = hashLazy (Botan.sha3 (224 :: Int))

sha3_224_hasher :: IO IncrementalHasher
sha3_224_hasher = mkIncrementalHasher (Botan.sha3 (224 :: Int))

sha3_256 :: L.ByteString -> HashDigest
sha3_256 = hashLazy (Botan.sha3 (256 :: Int))

sha3_256_hasher :: IO IncrementalHasher
sha3_256_hasher = mkIncrementalHasher (Botan.sha3 (256 :: Int))

sha3_384 :: L.ByteString -> HashDigest
sha3_384 = hashLazy (Botan.sha3 (384 :: Int))

sha3_384_hasher :: IO IncrementalHasher
sha3_384_hasher = mkIncrementalHasher (Botan.sha3 (384 :: Int))

sha3_512 :: L.ByteString -> HashDigest
sha3_512 = hashLazy (Botan.sha3 (512 :: Int))

sha3_512_hasher :: IO IncrementalHasher
sha3_512_hasher = mkIncrementalHasher (Botan.sha3 (512 :: Int))

skein512 :: L.ByteString -> HashDigest
skein512 = hashLazy Botan.Skein512

skein512_hasher :: IO IncrementalHasher
skein512_hasher = mkIncrementalHasher Botan.Skein512

blake2b_160 :: L.ByteString -> HashDigest
blake2b_160 = hashLazy (Botan.blake2b (160 :: Int))

blake2b_160_hasher :: IO IncrementalHasher
blake2b_160_hasher = mkIncrementalHasher (Botan.blake2b (160 :: Int))

blake2b_224 :: L.ByteString -> HashDigest
blake2b_224 = hashLazy (Botan.blake2b (224 :: Int))

blake2b_224_hasher :: IO IncrementalHasher
blake2b_224_hasher = mkIncrementalHasher (Botan.blake2b (224 :: Int))

blake2b_256 :: L.ByteString -> HashDigest
blake2b_256 = hashLazy (Botan.blake2b (256 :: Int))

blake2b_256_hasher :: IO IncrementalHasher
blake2b_256_hasher = mkIncrementalHasher (Botan.blake2b (256 :: Int))

blake2b_384 :: L.ByteString -> HashDigest
blake2b_384 = hashLazy (Botan.blake2b (384 :: Int))

blake2b_384_hasher :: IO IncrementalHasher
blake2b_384_hasher = mkIncrementalHasher (Botan.blake2b (384 :: Int))

blake2b_512 :: L.ByteString -> HashDigest
blake2b_512 = hashLazy (Botan.blake2b (512 :: Int))

blake2b_512_hasher :: IO IncrementalHasher
blake2b_512_hasher = mkIncrementalHasher (Botan.blake2b (512 :: Int))

md5 :: L.ByteString -> HashDigest
md5 = hashLazy Botan.MD5

md5_hasher :: IO IncrementalHasher
md5_hasher = mkIncrementalHasher Botan.MD5

md5s :: S.ByteString -> HashDigest
md5s = hashStrict Botan.MD5

hashLazy :: Botan.HashName -> L.ByteString -> HashDigest
hashLazy h b = unsafePerformIO $ do
	hasher <- Botan.hashInit h
	forM_ (L.toChunks b) (Botan.hashUpdate hasher)
	HashDigest <$> Botan.hashFinal hasher

hashStrict :: Botan.HashName -> S.ByteString -> HashDigest
hashStrict h b = unsafePerformIO $ do
	hasher <- Botan.hashInit h
	Botan.hashUpdate hasher b
	HashDigest <$> Botan.hashFinal hasher

mkIncrementalHasher :: Botan.HashName -> IO IncrementalHasher
mkIncrementalHasher h = do
	hasher <- Botan.hashInit h
	v <- newIORef (Just 0)
	return $ IncrementalHasher
		{ updateIncrementalHasher = \b -> do
			readIORef v >>= \case
				Just n -> do
					let !n' = n + fromIntegral (S.length b)
					writeIORef v (Just n')
					Botan.hashUpdate hasher b
				Nothing -> return ()
		, finalizeIncrementalHasher = do
			readIORef v >>= \case
				Just _ -> Just . HashDigest
					<$> Botan.hashFinal hasher
				Nothing -> return Nothing
		, unableIncrementalHasher = writeIORef v Nothing
		, positionIncrementalHasher = readIORef v
		}
