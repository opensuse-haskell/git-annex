{- Blake3 convenience wrapper with b3hash support.
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
	blake3File,
) where

import Utility.Hash.Types
import Utility.Hash.Incremental
import Utility.FileSystemEncoding
import Utility.OsPath
import Utility.Path
import Utility.FileSize
import Utility.Process
import Utility.Exception
import Utility.Monad

import qualified BLAKE3
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Control.Concurrent.MVar
import GHC.IO (unsafePerformIO)

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

{- When b3sum is in the path, this uses it to hash a file. For large
 - files, that is much faster due to supporting parallelism.
 -
 - This returns Nothing when b3sum is not in the path, or exits nonzero, 
 - or when the file is too small to be worth running it.
 -}
blake3File :: OsPath -> IO (Maybe Hash)
blake3File file = takeMVar b3sumAvailable >>= \case
	Just True -> do
		putMVar b3sumAvailable (Just True)
		runb3sum
	Just False -> do
		putMVar b3sumAvailable (Just False)
		return Nothing
	Nothing -> ifM (inSearchPath "b3sum")
		( do
			putMVar b3sumAvailable (Just True)
			runb3sum
		, do
			putMVar b3sumAvailable (Just False)
			return Nothing
		)
  where
	runb3sum = ifM filelargeenough
		( tryNonAsync runb3sum' >>= return . \case
			Right output -> case lines output of
				(hash:[]) | length hash == 64 ->
					Just $ Hash $ encodeBS hash
				_ -> Nothing
			Left _ ->  Nothing
		, return Nothing
		)
	runb3sum' = readProcess "b3sum" ["--no-names", "--", fromOsPath file]
	
	-- A file needs to be about 3mb in size before the overhead of
	-- starting a b3sum process is worthwhile.
	filelargeenough = tryNonAsync (getFileSize file) >>= return . \case
		Right sz -> sz > 3000000
		Left _ -> False

{- Used to avoid needing to check the PATH for b3sum each time
 - blake3File is called. -}
{-# NOINLINE b3sumAvailable #-}
b3sumAvailable :: MVar (Maybe Bool)
b3sumAvailable = unsafePerformIO $ newMVar Nothing
