{- git-annex assistant daemon
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Assistant where

import qualified Annex
import Assistant.Common
import Assistant.DaemonStatus
import Assistant.NamedThread
import Assistant.Types.ThreadedMonad
import Assistant.Threads.DaemonStatus
import Assistant.Threads.Watcher
import Assistant.Threads.Committer
import Assistant.Threads.Pusher
import Assistant.Threads.Exporter
import Assistant.Threads.Merger
import Assistant.Threads.TransferWatcher
import Assistant.Threads.Transferrer
import Assistant.Threads.RemoteControl
import Assistant.Threads.SanityChecker
import Assistant.Threads.Cronner
import Assistant.Threads.ProblemFixer
#ifndef mingw32_HOST_OS
import Assistant.Threads.MountWatcher
#endif
import Assistant.Threads.NetWatcher
import Assistant.Threads.Upgrader
import Assistant.Threads.UpgradeWatcher
import Assistant.Threads.TransferScanner
import Assistant.Threads.TransferPoller
import Assistant.Threads.ConfigMonitor
import Assistant.Threads.Glacier
#ifdef WITH_WEBAPP
import Assistant.WebApp
import Assistant.Threads.WebApp
#ifdef WITH_PAIRING
import Assistant.Threads.PairListener
#endif
#else
import Assistant.Types.UrlRenderer
#endif
import qualified Utility.Daemon
import Utility.ThreadScheduler
import Utility.HumanTime
import Annex.Perms
import Annex.BranchState
import Utility.LogFile
import Annex.Path
#ifdef mingw32_HOST_OS
import Utility.Env
import System.Environment (getArgs)
#endif
import qualified Utility.Debug as Debug

import Network.Socket (HostName, PortNumber)

stopDaemon :: Annex ()
stopDaemon = liftIO . Utility.Daemon.stopDaemon =<< fromRepo gitAnnexPidFile

{- Starts the daemon. If the daemon is run in the foreground, once it's
 - running, can start the browser.
 -
 - startbrowser is passed the url and html shim file, as well as the original
 - stdout and stderr descriptors. -}
startDaemon :: Bool -> Bool -> Maybe Duration -> Maybe String -> Maybe HostName -> Maybe PortNumber ->  Maybe (Maybe Handle -> Maybe Handle -> String -> OsPath -> IO ()) -> Annex ()
startDaemon assistant foreground startdelay cannotrun listenhost listenport startbrowser = do
	Annex.changeState $ \s -> s { Annex.daemon = True }
	enableInteractiveBranchAccess
	pidfile <- fromRepo gitAnnexPidFile
	logfile <- fromRepo gitAnnexDaemonLogFile
	liftIO $ Debug.debug "Assistant" $ "logging to " ++ fromOsPath logfile
	createAnnexDirectory (parentDir pidfile)
#ifndef mingw32_HOST_OS
	createAnnexDirectory (parentDir logfile)
	let logfd = handleToFd =<< openLog (fromOsPath logfile)
	if foreground
		then do
			origout <- liftIO $ catchMaybeIO $ 
				fdToHandle =<< dup stdOutput
			origerr <- liftIO $ catchMaybeIO $ 
				fdToHandle =<< dup stdError
			let undaemonize = Utility.Daemon.foreground logfd (Just pidfile)
			start undaemonize $ 
				case startbrowser of
					Nothing -> Nothing
					Just a -> Just $ a origout origerr
		else do
			git_annex <- fromOsPath <$> liftIO programPath
			ps <- gitAnnexDaemonizeParams
			start (Utility.Daemon.daemonize git_annex ps logfd (Just pidfile) False) Nothing
#else
	-- Windows doesn't daemonize, but does redirect output to the
	-- log file. The only way to do so is to restart the program.
	when (foreground || not foreground) $ do
		let flag = "GIT_ANNEX_OUTPUT_REDIR"
		createAnnexDirectory (parentDir logfile)
		ifM (liftIO $ isNothing <$> getEnv flag)
			( liftIO $ withNullHandle $ \nullh -> do
				loghandle <- openLog (fromOsPath logfile)
				e <- getEnvironment
				cmd <- fromOsPath <$> programPath
				ps <- getArgs
				let p = (proc cmd ps)
					{ env = Just (addEntry flag "1" e)
					, std_in = UseHandle nullh
					, std_out = UseHandle loghandle
					, std_err = UseHandle loghandle
					}
				exitcode <- withCreateProcess p $ \_ _ _ pid ->
					waitForProcess pid
				exitWith exitcode
			, start (Utility.Daemon.foreground (Just pidfile)) $
				case startbrowser of
					Nothing -> Nothing
					Just a -> Just $ a Nothing Nothing
			)
#endif
  where
	start daemonize webappwaiter = withThreadState $ \st -> do
		checkCanWatch
		dstatus <- startDaemonStatus
		logfile <- fromRepo gitAnnexDaemonLogFile
		liftIO $ Debug.debug "Assistant" $ "logging to " ++ fromOsPath logfile
		liftIO $ daemonize $
			flip runAssistant (go webappwaiter) 
				=<< newAssistantData st dstatus

#ifdef WITH_WEBAPP
	go webappwaiter = do
		d <- getAssistant id
#else
	go _webappwaiter = do
#endif
		urlrenderer <- liftIO newUrlRenderer
#ifdef WITH_WEBAPP
		let webappthread = [ assist $ webAppThread d urlrenderer False cannotrun Nothing listenhost listenport webappwaiter ]
#else
		let webappthread = []
#endif
		let threads = if isJust cannotrun
			then webappthread
			else webappthread ++
				[ watch commitThread
#ifdef WITH_WEBAPP
#ifdef WITH_PAIRING
				, assist $ pairListenerThread urlrenderer
#endif
#endif
				, assist pushThread
				, assist pushRetryThread
				, assist exportThread
				, assist exportRetryThread
				, assist mergeThread
				, assist transferWatcherThread
				, assist transferPollerThread
				, assist transfererThread
				, assist remoteControlThread
				, assist daemonStatusThread
				, assist $ sanityCheckerDailyThread urlrenderer
				, assist sanityCheckerHourlyThread
				, assist $ problemFixerThread urlrenderer
#ifndef mingw32_HOST_OS
				, assist $ mountWatcherThread urlrenderer
#endif
				, assist netWatcherThread
				, assist $ upgraderThread urlrenderer
				, assist $ upgradeWatcherThread urlrenderer
				, assist netWatcherFallbackThread
				, assist $ transferScannerThread urlrenderer
				, assist $ cronnerThread urlrenderer
				, assist configMonitorThread
				, assist glacierThread
				, watch watchThread
				-- must come last so that all threads that wait
				-- on it have already started waiting
				, watch $ sanityCheckerStartupThread startdelay
				]
	
		mapM_ (startthread urlrenderer) threads
		liftIO waitForTermination

	watch a = (True, a)
	assist a = (False, a)
	startthread urlrenderer (watcher, t)
		| watcher || assistant = startNamedThread urlrenderer t
		| otherwise = noop
