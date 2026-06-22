{- Blake3 convenience wrapper.
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 - Copyright 2022 edef <edef@edef.eu>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Utility.Hash.Blake3 (
	blake3,
	blake3IncrementalVerifier,
) where

import Utility.Hash.Types
import Utility.Hash.Incremental
import Utility.FileSystemEncoding

import qualified BLAKE3
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IORef

finalize :: BLAKE3.Hasher -> BLAKE3.Digest BLAKE3.DEFAULT_DIGEST_LEN
finalize = BLAKE3.finalize

blake3 :: L.ByteString -> Hash
blake3 = Hash . encodeBS . show . finalize . L.foldlChunks ((. pure) . BLAKE3.update) (BLAKE3.init Nothing)

blake3IncrementalVerifier :: String -> (Hash -> Bool) -> IO IncrementalVerifier
blake3IncrementalVerifier desc samechecksum = do
	v <- newIORef (Just (BLAKE3.init Nothing, 0))
	return $ IncrementalVerifier
		{ updateIncrementalVerifier = \b ->
			modifyIORef' v $ \case
				(Just (ctx', n)) ->
					let !ctx'' = BLAKE3.update ctx' [b]
					    !n' = n + fromIntegral (S.length b)
					in (Just (ctx'', n'))
				Nothing -> Nothing
		, finalizeIncrementalVerifier =
			fmap (samechecksum . Hash . encodeBS . show . finalize . fst) <$> readIORef v
		, unableIncrementalVerifier = writeIORef v Nothing
		, positionIncrementalVerifier = fmap snd <$> readIORef v
		, descIncrementalVerifier = desc
		}
