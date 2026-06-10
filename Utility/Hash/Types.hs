{- Hash types
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Hash.Types where

import qualified Data.ByteString as S
import Utility.FileSystemEncoding

newtype Hash = Hash S.ByteString

instance Show Hash where
	show (Hash v) = decodeBS v
