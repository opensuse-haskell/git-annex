{- misc utility functions
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Misc (
	hGetContentsStrict,
	separate,
	separate',
	separateEnd',
	firstLine,
	firstLine',
	fileLines,
	fileLines',
	linesFile,
	linesFile',
	segment,
	segmentDelim,
	massReplace,
	hGetSomeString,
	exitBool,

	prop_segment_regressionTest,
) where

import System.IO
import Control.Monad
import Foreign
import Data.Char
import Data.List
import System.Exit
import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Prelude

{- A version of hgetContents that is not lazy. Ensures file is 
 - all read before it gets closed. -}
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict = hGetContents >=> \s -> length s `seq` return s

{- Like break, but the item matching the condition is not included
 - in the second result list.
 -
 - separate (== ':') "foo:bar" = ("foo", "bar")
 - separate (== ':') "foobar" = ("foobar", "")
 -}
separate :: (a -> Bool) -> [a] -> ([a], [a])
separate c l = unbreak $ break c l
  where
	unbreak (a, (_:b)) = (a, b)
	unbreak r = r

separate' :: (Word8 -> Bool) -> S.ByteString -> (S.ByteString, S.ByteString)
separate' c l = unbreak $ S.break c l
  where
	unbreak r@(a, b)
		| S.null b = r
		| otherwise = (a, S.tail b)

separateEnd' :: (Word8 -> Bool) -> S.ByteString -> (S.ByteString, S.ByteString)
separateEnd' c l = unbreak $ S.breakEnd c l
  where
	unbreak r@(a, b)
		| S.null a = r
		| otherwise = (S.init a, b)

{- Breaks out the first line. -}
firstLine :: String -> String
firstLine = takeWhile (/= '\n')

firstLine' :: S.ByteString -> S.ByteString
firstLine' = S.takeWhile (/= nl)
  where
	nl = fromIntegral (ord '\n')

-- On windows, readFile does NewlineMode translation,
-- stripping CR before LF. When converting to ByteString,
-- use this to emulate that.
fileLines :: L.ByteString -> [L.ByteString]
#ifdef mingw32_HOST_OS
fileLines = map stripCR . L8.lines
  where
	stripCR b = case L8.unsnoc b of
		Nothing -> b
		Just (b', e)
			| e == '\r' -> b'
			| otherwise -> b
#else
fileLines = L8.lines
#endif

fileLines' :: S.ByteString -> [S.ByteString]
#ifdef mingw32_HOST_OS
fileLines' = map stripCR . S8.lines
  where
	stripCR b = case S8.unsnoc b of
		Nothing -> b
		Just (b', e)
			| e == '\r' -> b'
			| otherwise -> b
#else
fileLines' = S8.lines
#endif

-- One windows, writeFile does NewlineMode translation,
-- adding CR before LF. When converting to ByteString, use this to emulate that.
linesFile :: L.ByteString -> L.ByteString
#ifndef mingw32_HOST_OS
linesFile = id
#else
linesFile = L8.concat . concatMap (\x -> [x, L8.pack "\r\n"]) . fileLines
#endif

linesFile' :: S.ByteString -> S.ByteString
#ifndef mingw32_HOST_OS
linesFile' = id
#else
linesFile' = S8.concat . concatMap (\x -> [x, S8.pack "\r\n"]) . fileLines'
#endif

{- Splits a list into segments that are delimited by items matching
 - a predicate. (The delimiters are not included in the segments.)
 - Segments may be empty. -}
segment :: (a -> Bool) -> [a] -> [[a]]
segment p l = map reverse $ go [] [] l
  where
	go c r [] = reverse $ c:r
	go c r (i:is)
		| p i = go [] (c:r) is
		| otherwise = go (i:c) r is

prop_segment_regressionTest :: Bool
prop_segment_regressionTest = all id
	-- Even an empty list is a segment.
	[ segment (== "--") [] == [[]]
	-- There are two segments in this list, even though the first is empty.
	, segment (== "--") ["--", "foo", "bar"] == [[],["foo","bar"]]
	]

{- Includes the delimiters as segments of their own. -}
segmentDelim :: (a -> Bool) -> [a] -> [[a]]
segmentDelim p l = map reverse $ go [] [] l
  where
	go c r [] = reverse $ c:r
	go c r (i:is)
		| p i = go [] ([i]:c:r) is
		| otherwise = go (i:c) r is

{- Replaces multiple values in a string.
 -
 - Takes care to skip over just-replaced values, so that they are not
 - mangled. For example, massReplace [("foo", "new foo")] does not
 - replace the "new foo" with "new new foo".
 -}
massReplace :: [(String, String)] -> String -> String
massReplace vs = go [] vs
  where

	go acc _ [] = concat $ reverse acc
	go acc [] (c:cs) = go ([c]:acc) vs cs
	go acc ((val, replacement):rest) s
		| val `isPrefixOf` s =
			go (replacement:acc) vs (drop (length val) s)
		| otherwise = go acc rest s

{- Wrapper around hGetBufSome that returns a String.
 -
 - The null string is returned on eof, otherwise returns whatever
 - data is currently available to read from the handle, or waits for
 - data to be written to it if none is currently available.
 - 
 - Note on encodings: The normal encoding of the Handle is ignored;
 - each byte is converted to a Char. Not unicode clean!
 -}
hGetSomeString :: Handle -> Int -> IO String
hGetSomeString h sz = do
	fp <- mallocForeignPtrBytes sz
	len <- withForeignPtr fp $ \buf -> hGetBufSome h buf sz
	map (chr . fromIntegral) <$> withForeignPtr fp (peekbytes len)
  where
	peekbytes :: Int -> Ptr Word8 -> IO [Word8]
	peekbytes len buf = mapM (peekElemOff buf) [0..pred len]

exitBool :: Bool -> IO a
exitBool False = exitFailure
exitBool True = exitSuccess
