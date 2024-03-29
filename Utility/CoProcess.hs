{- Interface for running a shell command as a coprocess,
 - sending it queries and getting back results.
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.CoProcess (
	CoProcessHandle,
	CoProcessState(..),
	start,
	stop,
	query,
	send,
	receive,
) where

import Common

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO)

type CoProcessHandle = MVar CoProcessState

data CoProcessState = CoProcessState
	{ coProcessPid :: ProcessHandle
	, coProcessTo :: Handle
	, coProcessFrom :: Handle
	, coProcessSpec :: CoProcessSpec
	}

data CoProcessSpec = CoProcessSpec
	{ coProcessNumRestarts :: Int
	, coProcessCmd :: FilePath
	, coProcessParams :: [String]
	, coProcessEnv :: Maybe [(String, String)]
	}

start :: Int -> FilePath -> [String] -> Maybe [(String, String)] -> IO CoProcessHandle
start numrestarts cmd params environ = do
	s <- start' $ CoProcessSpec numrestarts cmd params environ
	newMVar s

start' :: CoProcessSpec -> IO CoProcessState
start' s = do
	(pid, from, to) <- startInteractiveProcess (coProcessCmd s) (coProcessParams s) (coProcessEnv s)
	rawMode from
	rawMode to
	return $ CoProcessState pid to from s
  where
#ifdef mingw32_HOST_OS
	rawMode h = hSetNewlineMode h noNewlineTranslation
#else
	rawMode _ = return ()
#endif

stop :: CoProcessHandle -> IO ()
stop ch = do
	s <- readMVar ch
	hClose $ coProcessTo s
	hClose $ coProcessFrom s
	let p = proc (coProcessCmd $ coProcessSpec s) (coProcessParams $ coProcessSpec s)
	forceSuccessProcess p (coProcessPid s)

{- To handle a restartable process, any IO exception thrown by the send and
 - receive actions are assumed to mean communication with the process
 - failed, and the failed action is re-run with a new process. -}
query :: (MonadIO m, MonadCatch m) => CoProcessHandle -> (Handle -> m a) -> (Handle -> m b) -> m b
query ch sender receiver = do
	s <- liftIO $ readMVar ch
	restartable s (sender $ coProcessTo s) $ const $
		restartable s (liftIO $ hFlush $ coProcessTo s) $ const $
			restartable s (receiver $ coProcessFrom s)
				return
  where
	restartable s a cont
		| coProcessNumRestarts (coProcessSpec s) > 0 =
			maybe restart cont =<< catchMaybeIO a
		| otherwise = cont =<< a
	restart = do
		s <- liftIO $ takeMVar ch
		void $ liftIO $ catchMaybeIO $ do
			hClose $ coProcessTo s
			hClose $ coProcessFrom s
		void $ liftIO $ waitForProcess $ coProcessPid s
		s' <- liftIO $ start' $ (coProcessSpec s)
			{ coProcessNumRestarts = coProcessNumRestarts (coProcessSpec s) - 1 }
		liftIO $ putMVar ch s'
		query ch sender receiver

send :: MonadIO m => CoProcessHandle -> (Handle -> m a) -> m a
send ch a = do
	s <- liftIO $ readMVar ch
	a (coProcessTo s)

receive :: MonadIO m => CoProcessHandle -> (Handle -> m a) -> m a
receive ch a = do
	s <- liftIO $ readMVar ch
	liftIO $ hFlush $ coProcessTo s
	a (coProcessFrom s)
