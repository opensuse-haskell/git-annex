{- thread scheduling
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.ThreadScheduler (
	SecondsDelay(..),
	MicrosecondsDelay,
	runEvery,
	threadDelaySeconds,
	waitForTermination,
	oneSecond,
	unboundDelay,
) where

import Control.Monad
import qualified Control.Concurrent.Thread.Delay as Unbounded
#ifndef mingw32_HOST_OS
import Control.Concurrent
import Control.Monad.IfElse
import System.Posix.IO
#endif
#ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.Posix.Terminal
#endif

newtype SecondsDelay = SecondsDelay { fromSecondsDelay :: Int }
	deriving (Eq, Ord, Show)

type MicrosecondsDelay = Integer

{- Runs an action repeatedly forever, sleeping at least the specified number
 - of seconds in between. -}
runEvery :: SecondsDelay -> IO a -> IO a
runEvery n a = forever $ do
	threadDelaySeconds n
	a

threadDelaySeconds :: SecondsDelay -> IO ()
threadDelaySeconds (SecondsDelay n) = unboundDelay (fromIntegral n * oneSecond)

{- Like threadDelay, but not bounded by an Int. -}
unboundDelay :: MicrosecondsDelay -> IO ()
unboundDelay = Unbounded.delay

{- Pauses the main thread, letting children run until program termination. -}
waitForTermination :: IO ()
waitForTermination = do
#ifdef mingw32_HOST_OS
	forever $ threadDelaySeconds (SecondsDelay 6000)
#else
	lock <- newEmptyMVar
	let check sig = void $
		installHandler sig (CatchOnce $ putMVar lock ()) Nothing
	check softwareTermination
	whenM (queryTerminal stdInput) $
		check keyboardSignal
	takeMVar lock
#endif

oneSecond :: MicrosecondsDelay
oneSecond = 1000000
