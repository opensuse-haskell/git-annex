{- Generates a NullSoft installer program for git-annex on Windows.
 -
 - This uses the Haskell nsis package to generate a .nsi file,
 - which is then used to produce git-annex-installer.exe
 - 
 - The installer includes git-annex, and utilities it uses, with the
 - exception of git and some utilities that are bundled with git.
 - The user needs to install git separately, and the installer checks
 - for that.
 - 
 - To build the installer, git-annex should already be built to
 - ./git-annex.exe, then run this program, using eg:
 -
 - stack ghc --no-haddock --package nsis Build/NullSoftInstaller.hs
 -
 - A build of libmagic will also be included in the installer, if its files
 - are found in the current directory: 
 -   ./magic.mgc ./libmagic-1.dll ./libgnurx-0.dll
 - To build git-annex to use libmagic, it has to be built with the
 - magicmime build flag turned on.
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Development.NSIS
import Control.Monad
import Control.Applicative
import Data.String
import Data.Maybe
import Data.Char
import Data.List (nub, isPrefixOf)

import Utility.Tmp.Dir
import Utility.Path
import Utility.CopyFile
import Utility.SafeCommand
import Utility.Process
import Utility.Exception
import Utility.Directory
import Utility.SystemDirectory
import Utility.OsPath
import Build.BundledPrograms

main = do
	withTmpDir "nsis-build" $ \tmpdir -> do
		let gitannex = fromOsPath $ tmpdir </> toOsPath gitannexprogram
		mustSucceed "ln" [File "git-annex.exe", File gitannex]
		magicDLLs' <- installwhenpresent magicDLLs tmpdir
		magicShare' <- installwhenpresent magicShare tmpdir
		let license = fromOsPath $ tmpdir </> toOsPath licensefile
		mustSucceed "sh" [Param "-c", Param $ "zcat standalone/licences.gz > '" ++ license ++ "'"]
		webappscript <- vbsLauncher tmpdir "git-annex-webapp" "git annex webapp"
		autostartscript <- vbsLauncher tmpdir "git-annex-autostart" "git annex assistant --autostart"
		let htmlhelp = fromOsPath $ tmpdir </> literalOsPath "git-annex.html"
		writeFile htmlhelp htmlHelpText
		let gitannexcmd = fromOsPath $ tmpdir </> literalOsPath "git-annex.cmd"
		writeFile gitannexcmd "git annex %*"
		writeFile nsifile $ makeInstaller
			gitannex gitannexcmd license htmlhelp (winPrograms ++ magicDLLs') magicShare'
			[ webappscript, autostartscript ]
		mustSucceed "makensis" [File nsifile]
	removeFile (toOsPath nsifile) -- left behind if makensis fails
  where
	nsifile = "git-annex.nsi"
	mustSucceed cmd params = do
		r <- boolSystem cmd params
		case r of
			True -> return ()
			False -> error $ cmd ++ " failed"
	installwhenpresent fs tmpdir = do
		fs' <- forM fs $ \f -> do
			present <- doesFileExist (toOsPath f)
			if present
				then do
					mustSucceed "ln" [File f, File (fromOsPath (tmpdir </> toOsPath f))]
					return (Just f)
				else return Nothing
		return (catMaybes fs')

{- Generates a .vbs launcher which runs a command without any visible DOS
 - box. It expects to be passed the directory where git-annex is installed. -}
vbsLauncher :: OsPath -> String -> String -> IO String
vbsLauncher tmpdir basename cmd = do
	let f = fromOsPath $ tmpdir </> toOsPath (basename ++ ".vbs")
	writeFile f $ unlines
		[ "Set objshell=CreateObject(\"Wscript.Shell\")"
		, "objShell.CurrentDirectory = Wscript.Arguments.item(0)"
		, "objShell.Run(\"" ++ cmd ++ "\"), 0, False"
		]
	return f

gitannexprogram :: FilePath
gitannexprogram = "git-annex.exe"

licensefile :: FilePath
licensefile = "git-annex-licenses.txt"

installer :: FilePath
installer = "git-annex-installer.exe"

uninstaller :: FilePath
uninstaller = "git-annex-uninstall.exe"

gitInstallDir32 :: Exp FilePath
gitInstallDir32 = fromString "$PROGRAMFILES\\Git"

gitInstallDir64 :: Exp FilePath
gitInstallDir64 = fromString "$PROGRAMFILES64\\Git"

gitInstallDir :: Exp FilePath
gitInstallDir = gitInstallDir64

-- This intentionally has a different name than git-annex or
-- git-annex-webapp, since it is itself treated as an executable file.
-- Also, on XP, the filename is displayed, not the description.
startMenuItem :: Exp FilePath
startMenuItem = "$SMPROGRAMS/Git Annex (Webapp).lnk"

oldStartMenuItem :: Exp FilePath
oldStartMenuItem = "$SMPROGRAMS/git-annex.lnk"

autoStartItem :: Exp FilePath
autoStartItem = "$SMSTARTUP/git-annex-autostart.lnk"

needGit :: Exp String
needGit = strConcat
	[ fromString "You need git installed to use git-annex. Looking at "
	, gitInstallDir
	, fromString " , it seems to not be installed, "
	, fromString "or may be installed in another location. "
	, fromString "You can install git from http:////git-scm.com//"
	]

makeInstaller :: FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> String
makeInstaller gitannex gitannexcmd license htmlhelp extrabins sharefiles launchers = nsis $ do
	name "git-annex"
	outFile $ str installer
	{- Installing into the same directory as git avoids needing to modify
	 - path myself, since the git installer already does it. -}
	installDir gitInstallDir
	requestExecutionLevel Admin

	iff (fileExists gitInstallDir)
		(return ())
		(alert needGit)
	
	-- Pages to display
	page Directory                   -- Pick where to install
	page (License license)
	page InstFiles                   -- Give a progress bar while installing
	-- Start menu shortcut
	Development.NSIS.createDirectory "$SMPROGRAMS"
	createShortcut startMenuItem
		[ Target "wscript.exe"
		, Parameters "\"$INSTDIR/cmd/git-annex-webapp.vbs\" \"$INSTDIR/cmd\""
		, StartOptions "SW_SHOWNORMAL"
		, IconFile "$INSTDIR/usr/bin/git-annex.exe"
		, IconIndex 2
		, Description "Git Annex (Webapp)"
		]
	delete [RebootOK] $ oldStartMenuItem
	createShortcut autoStartItem
		[ Target "wscript.exe"
		, Parameters "\"$INSTDIR/cmd/git-annex-autostart.vbs\" \"$INSTDIR/cmd\""
		, StartOptions "SW_SHOWNORMAL"
		, IconFile "$INSTDIR/usr/bin/git-annex.exe"
		, IconIndex 2
		, Description "git-annex autostart"
		]
	section "cmd" [] $ do
		-- Remove old files no longer installed in the cmd
		-- directory.
		removefilesFrom "$INSTDIR/cmd" (gitannex:extrabins)
		-- Install everything to the same location git puts its
		-- bins. This makes "git annex" work in the git bash
		-- shell, since git expects to find the git-annex binary
		-- there.
		setOutPath "$INSTDIR\\usr\\bin"
		mapM_ addfile (gitannex:extrabins)
		setOutPath "$INSTDIR\\usr\\share\\misc"
		mapM_ addfile sharefiles
		-- This little wrapper is installed in the cmd directory,
		-- so that "git-annex" works (as well as "git annex"),
		-- when only that directory is in PATH (ie, in a ms-dos
		-- prompt window).
		setOutPath "$INSTDIR\\cmd"
		addfile gitannexcmd
	section "meta" [] $ do
		-- git opens this file when git annex --help is run.
		-- (Program Files/Git/mingw64/share/doc/git-doc/git-annex.html)
		setOutPath "$INSTDIR\\mingw64\\share\\doc\\git-doc"
		addfile htmlhelp
		setOutPath "$INSTDIR"
		addfile license
		setOutPath "$INSTDIR\\cmd"
		mapM_ addfile launchers
		writeUninstaller $ str uninstaller
	uninstall $ do
		delete [RebootOK] $ startMenuItem
		delete [RebootOK] $ autoStartItem
		removefilesFrom "$INSTDIR/usr/bin" (gitannex:extrabins)
		removefilesFrom "$INSTDIR/cmd" (gitannexcmd:launchers)
		removefilesFrom "$INSTDIR\\mingw64\\share\\doc\\git-doc" [htmlhelp]
		removefilesFrom "$INSTDIR" [license, uninstaller]
  where
	addfile f = file [] (str f)
	removefilesFrom d = mapM_ (\f -> delete [RebootOK] $ fromString $ d ++ "/" ++ fromOsPath (takeFileName (toOsPath f)))

winPrograms :: [FilePath]
winPrograms = map (\p -> p ++ ".exe") bundledPrograms

htmlHelpText :: String
htmlHelpText = unlines
	[ "<html>"
	, "<title>git-annex help</title>"
	, "<body>"
	, "For help on git-annex, run \"git annex help\", or"
	, "<a href=\"https://git-annex.branchable.com/git-annex/\">read the man page</a>."
	, "</body>"
	, "</html"
	]
