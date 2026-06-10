{- Hash types
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Hash.Types where

import qualified Data.ByteString as S
import Data.Base16.Types
import Utility.FileSystemEncoding

newtype Hash = Hash (Base16 S.ByteString)

instance Show Hash where
	show (Hash v) = decodeBS (extractBase16 v)


