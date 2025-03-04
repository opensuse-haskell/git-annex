{- Assistant installation
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Assistant.Install where

import Assistant.Common
import Assistant.Install.AutoStart
import Config.Files
import Utility.FileMode
import Utility.Shell
import Utility.Tmp
import Utility.Env
import Utility.SshConfig
import qualified Utility.FileIO as F

#ifdef darwin_HOST_OS
import Utility.OSX
#else
import Utility.FreeDesktop
import Assistant.Install.Menu
import Utility.UserInfo
import Utility.Android
#endif

import System.PosixCompat.Files (ownerExecuteMode)
import qualified Data.ByteString.Char8 as S8

standaloneAppBase :: IO (Maybe OsPath)
standaloneAppBase = fmap toOsPath <$> getEnv "GIT_ANNEX_APP_BASE"

{- The standalone app does not have an installation process.
 - So when it's run, it needs to set up autostarting of the assistant
 - daemon, as well as writing the programFile, and putting the
 - git-annex-shell and git-annex-wrapper wrapper scripts into ~/.ssh
 -
 - Note that this is done every time it's started, so if the user moves
 - it around, the paths this sets up won't break.
 -
 - File manager hook script installation is done even for
 - packaged apps, since it has to go into the user's home directory.
 -}
ensureInstalled :: IO ()
ensureInstalled = ifM (isJust <$> getEnv "GIT_ANNEX_PACKAGE_INSTALL")
	( go Nothing
	, go =<< standaloneAppBase
	)
  where
	go Nothing = installFileManagerHooks (literalOsPath "git-annex")
	go (Just base) = do
		let program = base </> literalOsPath "git-annex"
		programfile <- programFile
		createDirectoryIfMissing True (parentDir programfile)
		writeFile (fromOsPath programfile) (fromOsPath program)

#ifdef darwin_HOST_OS
		autostartfile <- userAutoStart osxAutoStartLabel
		installAutoStart (fromOsPath program) autostartfile
#else
		ifM osAndroid
			( do
				-- Integration with the Termux:Boot app.
				home <- myHomeDir
				let bootfile = toOsPath home </> literalOsPath ".termux" </> literalOsPath "boot" </> literalOsPath "git-annex"
				unlessM (doesFileExist bootfile) $ do
					createDirectoryIfMissing True (takeDirectory bootfile)
					writeFile (fromOsPath bootfile) "git-annex assistant --autostart"
			, do
				menufile <- desktopMenuFilePath "git-annex" <$> userDataDir
				icondir <- iconDir <$> userDataDir
				installMenu (fromOsPath program) menufile base icondir
				autostartfile <- autoStartPath "git-annex" <$> userConfigDir
				installAutoStart (fromOsPath program) autostartfile
			)
#endif

		sshdir <- sshDir
		let runshell var = "exec " ++ fromOsPath (base </> literalOsPath "runshell ") ++ var
		let rungitannexshell var = runshell $ "git-annex-shell -c \"" ++ var ++ "\""

		installWrapper (sshdir </> literalOsPath "git-annex-shell") $
			[ shebang
			, "set -e"
			, "if [ \"x$SSH_ORIGINAL_COMMAND\" != \"x\" ]; then"
			,   rungitannexshell "$SSH_ORIGINAL_COMMAND"
			, "else"
			,   rungitannexshell "$@"
			, "fi"
			]
		installWrapper (sshdir </> literalOsPath "git-annex-wrapper") $
			[ shebang
			, "set -e"
			, runshell "\"$@\""
			]

		installFileManagerHooks program

installWrapper :: OsPath -> [String] -> IO ()
installWrapper file content = do
	let content' = map encodeBS content
	curr <- catchDefaultIO [] $ fileLines' <$> F.readFile' file
	when (curr /= content') $ do
		createDirectoryIfMissing True (parentDir file)
		viaTmp F.writeFile' file $ linesFile' (S8.unlines content')
		modifyFileMode file $ addModes [ownerExecuteMode]

installFileManagerHooks :: OsPath -> IO ()
#ifdef linux_HOST_OS
installFileManagerHooks program = unlessM osAndroid $ do
	let actions = ["get", "drop", "undo"]

	-- Gnome
	nautilusScriptdir <- (\d -> d </> literalOsPath "nautilus" </> literalOsPath "scripts") <$> userDataDir
	createDirectoryIfMissing True nautilusScriptdir
	forM_ actions $
		genNautilusScript nautilusScriptdir

	-- KDE
	userdata <- userDataDir
	let kdeServiceMenusdir = userdata </> literalOsPath "kservices5" </> literalOsPath "ServiceMenus"
	createDirectoryIfMissing True kdeServiceMenusdir
	writeFile (fromOsPath (kdeServiceMenusdir </> literalOsPath "git-annex.desktop"))
		(kdeDesktopFile actions)
  where
	genNautilusScript scriptdir action =
		installscript (scriptdir </> toOsPath (scriptname action)) $ unlines
			[ shebang
			, autoaddedcomment
			, "exec " ++ fromOsPath program ++ " " ++ action ++ " --notify-start --notify-finish -- \"$@\""
			]
	scriptname action = "git-annex " ++ action
	installscript f c = whenM (safetoinstallscript f) $ do
		writeFile (fromOsPath f) c
		modifyFileMode f $ addModes [ownerExecuteMode]
	safetoinstallscript f = catchDefaultIO True $
		elem (encodeBS autoaddedcomment) . fileLines'
			<$> F.readFile' f
	autoaddedcomment = "# " ++ autoaddedmsg ++ " (To disable, chmod 600 this file.)"
	autoaddedmsg = "Automatically added by git-annex, do not edit."

	kdeDesktopFile actions = unlines $ concat $
		kdeDesktopHeader actions : map kdeDesktopAction actions
	kdeDesktopHeader actions =
		[ "# " ++ autoaddedmsg
		, "[Desktop Entry]"
		, "Type=Service"
		, "ServiceTypes=all/allfiles"
		, "MimeType=all/all;"
		, "Actions=" ++ intercalate ";" (map kdeDesktopSection actions)
		, "X-KDE-Priority=TopLevel"
		, "X-KDE-Submenu=Git-Annex"
		, "X-KDE-Icon=git-annex"
		, "X-KDE-ServiceTypes=KonqPopupMenu/Plugin"
		]
	kdeDesktopSection command = "GitAnnex" ++ command
	kdeDesktopAction command = 
		[ ""
		, "[Desktop Action " ++ kdeDesktopSection command ++ "]"
		, "Name=" ++ command
		, "Icon=git-annex"
		, unwords
			[ "Exec=sh -c 'cd \"$(dirname \"$1\")\" &&"
			, fromOsPath program
			, command
			, "--notify-start --notify-finish -- \"$1\"'"
			, "false" -- this becomes $0 in sh, so unused
			, "%f"
			]
		]
#else
installFileManagerHooks _ = noop
#endif
