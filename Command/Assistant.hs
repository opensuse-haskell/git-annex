{- git-annex assistant
 -
 - Copyright 2012-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Assistant where

import Command
import qualified Command.Watch
import Annex.Init
import Annex.Path
import Config.Files
import Config.Files.AutoStart
import qualified BuildInfo
import Utility.HumanTime
import Assistant.Install
import Remote.List
import Annex.Startup

import Control.Concurrent.Async

cmd :: Command
cmd = dontCheck repoExists $ notBareRepo $
	noRepo (startNoRepo <$$> optParser) $
		command "assistant" SectionCommon
			"daemon to add files and automatically sync changes"
			paramNothing (seek <$$> optParser)

data AssistantOptions = AssistantOptions
	{ daemonOptions :: DaemonOptions
	, autoStartOption :: Bool
	, startDelayOption :: Maybe Duration
	, autoStopOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser AssistantOptions
optParser _ = AssistantOptions
	<$> parseDaemonOptions True
	<*> switch
		( long "autostart"
		<> help "start in known repositories"
		)
	<*> optional (option (eitherReader parseDuration)
		( long "startdelay" <> metavar paramNumber
		<> help "delay before running startup scan"
		))
	<*> switch
		( long "autostop"
		<> help "stop in known repositories"
		)

seek :: AssistantOptions -> CommandSeek
seek = commandAction . start

start :: AssistantOptions -> CommandStart
start o
	| autoStartOption o = do
		liftIO $ autoStart o
		stop
	| autoStopOption o = do
		liftIO autoStop
		stop
	| otherwise = do
		liftIO ensureInstalled
		ensureInitialized startupAnnex remoteList
		Command.Watch.start True (daemonOptions o) (startDelayOption o)

startNoRepo :: AssistantOptions -> IO ()
startNoRepo o
	| autoStartOption o = autoStart o
	| autoStopOption o = autoStop
	| otherwise = giveup "Not in a git repository."

-- Does not return
autoStart :: AssistantOptions -> IO ()
autoStart o = do
	dirs <- liftIO readAutoStartFile
	when (null dirs) $ do
		f <- autoStartFile
		giveup $ "Nothing listed in " ++ fromOsPath f
	program <- fromOsPath <$> programPath
	haveionice <- pure BuildInfo.ionice <&&> inSearchPath "ionice"
	pids <- forM dirs $ \d -> do
		putStrLn $ "git-annex autostart in " ++ fromOsPath d
		mpid <- catchMaybeIO $ go haveionice program d
		if foregroundDaemonOption (daemonOptions o)
			then return mpid
			else do
				case mpid of
					Nothing -> putStrLn "failed"
					Just pid -> ifM (checkSuccessProcess pid)
						( putStrLn "ok"
						, putStrLn "failed"
						)
				return Nothing
	-- Wait for any foreground jobs to finish and propagate exit status.
	ifM (all (== True) <$> mapConcurrently checkSuccessProcess (catMaybes pids))
		( exitSuccess
		, exitFailure
		)
  where
	go haveionice program dir = do
		setCurrentDirectory dir
		-- First stop any old daemon running in this directory, which
		-- might be a leftover from an old login session. Such a
		-- leftover might be left in an environment where it is
		-- unable to use the ssh agent or other login session
		-- resources.
		void $ boolSystem program [Param "assistant", Param "--stop"]
		(Nothing, Nothing, Nothing, pid) <- createProcess p
		return pid
	  where
		p
			| haveionice = proc "ionice"
				(toCommand $ Param "-c3" : Param program : baseparams)
			| otherwise = proc program
				(toCommand baseparams)
		baseparams = catMaybes
			[ Just $ Param "assistant"
			, Just $ Param $ "--startdelay=" ++ fromDuration (fromMaybe (Duration 5) (startDelayOption o))
			, if foregroundDaemonOption (daemonOptions o)
				then Just $ Param "--foreground"
				else Nothing
			]

autoStop :: IO ()
autoStop = do
	dirs <- liftIO readAutoStartFile
	program <- fromOsPath <$> programPath
	forM_ dirs $ \d -> do
		putStrLn $ "git-annex autostop in " ++ fromOsPath d
		tryIO (setCurrentDirectory d) >>= \case
			Right () -> ifM (boolSystem program [Param "assistant", Param "--stop"])
				( putStrLn "ok"
				, putStrLn "failed"
				)
			Left e -> do
				putStrLn (show e)
				putStrLn "failed"
