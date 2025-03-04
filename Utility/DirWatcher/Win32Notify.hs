{- Win32-notify interface
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.DirWatcher.Win32Notify (watchDir) where

import Common
import Utility.DirWatcher.Types
import qualified Utility.RawFilePath as R

import System.Win32.Notify
import System.PosixCompat.Files (isRegularFile)

watchDir :: OsPath -> (OsPath -> Bool) -> Bool -> WatchHooks -> IO WatchManager
watchDir dir ignored scanevents hooks = do
	scan dir
	wm <- initWatchManager
	void $ watchDirectory wm (fromOsPath dir) True [Create, Delete, Modify, Move] dispatch
	return wm
  where
	dispatch evt
		| ignoredPath ignored (toOsPath (filePath evt)) = noop
		| otherwise = case evt of
			(Deleted _ _)
				| isDirectory evt -> runhook delDirHook Nothing
				| otherwise -> runhook delHook Nothing
			(Created _ _)
				| isDirectory evt -> noop
				| otherwise -> runhook addHook Nothing
			(Modified _ _)
				| isDirectory evt -> noop
				{- Add hooks are run when a file is modified for 
				 - compatibility with INotify, which calls the add
				 - hook when a file is closed, and so tends to call
				 - both add and modify for file modifications. -}
				| otherwise -> do
					runhook addHook Nothing
					runhook modifyHook Nothing
	  where
		runhook h s = maybe noop (\a -> a (toOsPath (filePath evt)) s) (h hooks)

	scan d = unless (ignoredPath ignored d) $
		mapM_ go =<< emptyWhenDoesNotExist
			(dirContentsRecursiveSkipping (const False) False d)
	  where		
		go f
			| ignoredPath ignored f = noop
			| otherwise = do
				ms <- getstatus f
				case ms of
					Nothing -> noop
					Just s
						| isRegularFile s ->
							when scanevents $
								runhook addHook ms
						| otherwise ->
							noop
		  where
			runhook h s = maybe noop (\a -> a f s) (h hooks)
		
	getstatus = catchMaybeIO . R.getFileStatus . fromOsPath

{- Check each component of the path to see if it's ignored. -}
ignoredPath :: (OsPath -> Bool) -> OsPath -> Bool
ignoredPath ignored = any ignored . map dropTrailingPathSeparator . splitPath
