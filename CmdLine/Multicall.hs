{- git-annex multicall binary
 -
 - Copyright 2024-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module CmdLine.Multicall where


import Common

import System.Environment (getProgName)
import qualified Data.Map as M
#ifdef mingw32_HOST_OS
import Data.Char
#endif

-- Commands besides git-annex that can be run by the multicall binary.
--
-- The reason git-annex itself is not included here is because the program
-- can be renamed to any other name than these and will behave the same as
-- git-annex.
data OtherMultiCallCommand
	= GitAnnexShell
	| GitRemoteAnnex
	| GitRemoteP2PAnnex
	| GitRemoteTorAnnex
	| NullSoftInstaller

otherMulticallCommands :: M.Map String OtherMultiCallCommand
otherMulticallCommands = M.fromList
	[ ("git-annex-shell", GitAnnexShell)
	, ("git-remote-annex", GitRemoteAnnex)
	, ("git-remote-p2p-annex", GitRemoteP2PAnnex)
	, ("git-remote-tor-annex", GitRemoteTorAnnex)
	, ("nullsoft-installer", NullSoftInstaller)
	]

getCommandName :: IO String
getCommandName = do
#ifdef mingw32_HOST_OS
	n <- progname
	return $ map toLower $ takeWhile (/= '.') n
#else
	progname
#endif
  where
	progname = fromOsPath . takeFileName . toOsPath <$> getProgName
