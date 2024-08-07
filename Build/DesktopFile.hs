{- Generating and installing a desktop menu entry file and icon,
 - and a desktop autostart file. (And OSX equivalents.)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Build.DesktopFile where

import Utility.Exception
import Utility.FreeDesktop
import Utility.Path
import Utility.Monad
import Utility.SystemDirectory
import Utility.FileSystemEncoding
import Config.Files
import Utility.OSX
import Assistant.Install.AutoStart
import Assistant.Install.Menu

import System.Environment
#ifndef mingw32_HOST_OS 
import System.Posix.User
import Data.Maybe
import Control.Applicative
import Prelude
#endif

systemwideInstall :: IO Bool
#ifndef mingw32_HOST_OS 
systemwideInstall = isroot <||> (not <$> userdirset)
  where
	isroot = do
		uid <- fromIntegral <$> getRealUserID
		return $ uid == (0 :: Int)
	userdirset = isJust <$> catchMaybeIO (getEnv "USERDIR")
#else
systemwideInstall = return False
#endif

inDestDir :: FilePath -> IO FilePath
inDestDir f = do
	destdir <- catchDefaultIO "" (getEnv "DESTDIR")
	return $ destdir ++ "/" ++ f

writeFDODesktop :: FilePath -> IO ()
writeFDODesktop command = do
	systemwide <- systemwideInstall

	datadir <- if systemwide then return systemDataDir else userDataDir
	menufile <- inDestDir (desktopMenuFilePath "git-annex" datadir)
	icondir <- inDestDir (iconDir datadir)
	installMenu command menufile "doc" icondir

	configdir <- if systemwide then return systemConfigDir else userConfigDir
	installAutoStart command 
		=<< inDestDir (autoStartPath "git-annex" configdir)

writeOSXDesktop :: FilePath -> IO ()
writeOSXDesktop command = do
	installAutoStart command =<< inDestDir =<< ifM systemwideInstall
		( return $ systemAutoStart osxAutoStartLabel
		, userAutoStart osxAutoStartLabel
		)

install :: FilePath -> IO ()
install command = do
#ifdef darwin_HOST_OS
	writeOSXDesktop command
#else
	writeFDODesktop command
#endif
	ifM systemwideInstall
		( return ()
		, do
			programfile <- inDestDir =<< programFile
			createDirectoryIfMissing True (fromRawFilePath (parentDir (toRawFilePath programfile)))
			writeFile programfile command
		)

installUser :: FilePath -> IO ()
installUser command = ifM systemwideInstall
	( return ()
	, install command
	)
