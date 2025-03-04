{- OsPath utilities
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.OsPath (
	OsPath,
	OsString,
	literalOsPath,
	stringToOsPath,
	toOsPath,
	fromOsPath,
	module X,
	getSearchPath,
	unsafeFromChar,
) where

import Utility.FileSystemEncoding
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Lazy as L
#ifdef WITH_OSPATH
import System.OsPath as X hiding (OsPath, OsString, pack, unpack, unsafeFromChar)
import System.OsPath
import "os-string" System.OsString.Internal.Types
import qualified System.FilePath as PS
#if defined(mingw32_HOST_OS)
import GHC.IO (unsafePerformIO)
import System.OsString.Encoding.Internal (cWcharsToChars_UCS2)
import qualified System.OsString.Data.ByteString.Short.Word16 as BS16
#endif
#else
import System.FilePath.ByteString as X hiding (RawFilePath, getSearchPath)
import System.FilePath.ByteString (getSearchPath)
import Data.ByteString (ByteString)
import Data.Char
import Data.Word
#endif

class OsPathConv t where
	toOsPath :: t -> OsPath
	fromOsPath :: OsPath -> t

instance OsPathConv FilePath where
	toOsPath = toOsPath . toRawFilePath
	fromOsPath = fromRawFilePath . fromOsPath

#ifdef WITH_OSPATH
instance OsPathConv RawFilePath where
#if defined(mingw32_HOST_OS)
	toOsPath = bytesToOsPath
	fromOsPath = bytesFromOsPath
#else
	toOsPath = bytesToOsPath . S.toShort
	fromOsPath = S.fromShort . bytesFromOsPath
#endif

instance OsPathConv ShortByteString where
#if defined(mingw32_HOST_OS)
	toOsPath = bytesToOsPath . S.fromShort
	fromOsPath = S.toShort . bytesFromOsPath
#else
	toOsPath = bytesToOsPath
	fromOsPath = bytesFromOsPath
#endif

instance OsPathConv L.ByteString where
	toOsPath = toOsPath . L.toStrict
	fromOsPath = L.fromStrict . fromOsPath

#if defined(mingw32_HOST_OS)
-- On Windows, OsString contains a ShortByteString that is
-- utf-16 encoded. But the input RawFilePath is assumed to
-- be utf-8. So this is a relatively  expensive conversion.
bytesToOsPath :: RawFilePath -> OsPath
bytesToOsPath = unsafePerformIO . encodeFS . fromRawFilePath
#else
bytesToOsPath :: ShortByteString -> OsPath
bytesToOsPath = OsString . PosixString
#endif

#if defined(mingw32_HOST_OS)
bytesFromOsPath :: OsPath -> RawFilePath
-- On Windows, OsString contains a ShortByteString that is
-- utf-16 encoded, but RawFilePath is utf-8.
-- So this is relatively expensive conversion.
bytesFromOsPath = toRawFilePath . cWcharsToChars_UCS2 . BS16.unpack . getWindowsString . getOsString
#else
bytesFromOsPath :: OsPath -> ShortByteString
bytesFromOsPath = getPosixString . getOsString
#endif

{- For some reason not included in System.OsPath -}
getSearchPath :: IO [OsPath]
getSearchPath = map toOsPath <$> PS.getSearchPath

{- Used for string constants. Note that when using OverloadedStrings,
 - the IsString instance for ShortByteString only works properly with
 - ASCII characters. -}
literalOsPath :: ShortByteString -> OsPath
literalOsPath = toOsPath

#else
{- When not building with WITH_OSPATH, use RawFilePath.
 -}
type OsPath = RawFilePath

type OsString = ByteString

instance OsPathConv RawFilePath where
	toOsPath = id
	fromOsPath = id

instance OsPathConv ShortByteString where
	toOsPath = S.fromShort
	fromOsPath = S.toShort

instance OsPathConv L.ByteString where
	toOsPath = L.toStrict
	fromOsPath = L.fromStrict

unsafeFromChar :: Char -> Word8
unsafeFromChar = fromIntegral . ord

literalOsPath :: RawFilePath -> OsPath
literalOsPath = id
#endif

stringToOsPath :: String -> OsPath
stringToOsPath = toOsPath
