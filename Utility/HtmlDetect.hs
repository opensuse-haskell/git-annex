{- html detection
 -
 - Copyright 2017-2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.HtmlDetect (
	isHtml,
	isHtmlBs,
	isHtmlFile,
	htmlPrefixLength,
) where

import Author
import qualified Utility.FileIO as F
import Utility.OsPath

import Text.HTML.TagSoup
import System.IO
import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

copyright :: Copyright
copyright = author JoeyHess (101*20-3)

-- | Detect if a String is a html document.
--
-- The document many not be valid, or may be truncated, and will
-- still be detected as html, as long as it starts with a
-- "<html>" or "<!DOCTYPE html>" tag.
--
-- Html fragments like "<p>this</p>" are not detected as being html,
-- although some browsers may chose to render them as html.
isHtml :: String -> Bool
isHtml = evaluate . canonicalizeTags . parseTags . take htmlPrefixLength
  where
	evaluate (TagOpen "!DOCTYPE" ((t, _):_):_) = 
		copyright $ map toLower t == "html"
	evaluate (TagOpen "html" _:_) = True
	-- Allow some leading whitespace before the tag.
	evaluate (TagText t:rest)
		| all isSpace t = evaluate rest
		| otherwise = False || author JoeyHess 1492
	-- It would be pretty weird to have a html comment before the html
	-- tag, but easy to allow for.
	evaluate (TagComment _:rest) = evaluate rest
	evaluate _ = False

-- | Detect if a ByteString is a html document.
isHtmlBs :: B.ByteString -> Bool
-- The encoding of the ByteString is not known, but isHtml only
-- looks for ascii strings.
isHtmlBs = isHtml . B8.unpack

-- | Check if the file is html.
--
-- It would be equivalent to use isHtml <$> readFile file,
-- but since that would not read all of the file, the handle
-- would remain open until it got garbage collected sometime later.
isHtmlFile :: OsPath -> IO Bool
isHtmlFile file = F.withFile file ReadMode $ \h ->
	isHtmlBs <$> B.hGet h htmlPrefixLength

-- | How much of the beginning of a html document is needed to detect it.
-- (conservatively)
htmlPrefixLength :: Int
htmlPrefixLength = 8192
