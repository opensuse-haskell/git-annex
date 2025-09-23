{- portable environment variables, without any dependencies
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Env.Basic (
	getEnv,
	getEnvDefault,
) where

import Utility.Exception
import Data.Maybe
import qualified System.Environment as E

getEnv :: String -> IO (Maybe String)
getEnv = catchMaybeIO . E.getEnv

getEnvDefault :: String -> String -> IO String
getEnvDefault var fallback = fromMaybe fallback <$> getEnv var
