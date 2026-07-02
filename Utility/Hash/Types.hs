{- Hash types
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

module Utility.Hash.Types where

import qualified Data.ByteString as S
import "memory" Data.ByteArray
import qualified "memory" Data.ByteArray.Encoding as BAE
import Data.String
import Control.DeepSeq
import GHC.Generics
import Prelude hiding (length)

import Utility.FileSystemEncoding

-- base16 encoded hash
newtype Hash = Hash { hashByteString :: S.ByteString }	
	deriving (Eq, Generic)

instance Show Hash where
	show (Hash v) = decodeBS v

instance IsString Hash where
	fromString = Hash . encodeBS

instance NFData Hash

-- the raw hash digest
newtype HashDigest = HashDigest S.ByteString
	deriving (Eq, Generic)

digestToHash :: HashDigest -> Hash
digestToHash (HashDigest d) = Hash $ BAE.convertToBase BAE.Base16 d

instance NFData HashDigest

instance ByteArrayAccess HashDigest where
	length (HashDigest d) = length d
	withByteArray (HashDigest d) = withByteArray d
