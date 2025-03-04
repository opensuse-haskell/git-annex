{- File size.
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.FileSize (
	FileSize,
	getFileSize,
	getFileSize',
) where

#ifdef mingw32_HOST_OS
import Control.Exception (bracket)
import System.IO
import qualified Utility.FileIO as F
#else
import System.PosixCompat.Files (fileSize)
import qualified Utility.RawFilePath as R
#endif
import System.PosixCompat.Files (FileStatus)
import Utility.OsPath

type FileSize = Integer

{- Gets the size of a file.
 -
 - This is better than using fileSize, because on Windows that returns a
 - FileOffset which maxes out at 2 gb.
 - See https://github.com/jystic/unix-compat/issues/16
 -}
getFileSize :: OsPath -> IO FileSize
#ifndef mingw32_HOST_OS
getFileSize f = fmap (fromIntegral . fileSize) (R.getFileStatus (fromOsPath f))
#else
getFileSize f = bracket (F.openFile f ReadMode) hClose hFileSize
#endif

{- Gets the size of the file, when its FileStatus is already known.
 -
 - On windows, uses getFileSize. Otherwise, the FileStatus contains the
 - size, so this does not do any work. -}
getFileSize' :: OsPath -> FileStatus -> IO FileSize
#ifndef mingw32_HOST_OS
getFileSize' _ s = return $ fromIntegral $ fileSize s
#else
getFileSize' f _ = getFileSize f
#endif
