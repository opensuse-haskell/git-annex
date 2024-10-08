{- git autocorrection using Damerau-Levenshtein edit distance
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.AutoCorrect where

import Common
import Git.Types
import qualified Git.Config

import Text.EditDistance
import Control.Concurrent
import qualified Data.List.NonEmpty as NE

{- These are the same cost values as used in git. -}
gitEditCosts :: EditCosts
gitEditCosts = EditCosts
	{ deletionCosts = ConstantCost 4
	, insertionCosts = ConstantCost 1
	, substitutionCosts = ConstantCost 2
	, transpositionCosts = ConstantCost 0
	}
		
{- Git's source calls this "an empirically derived magic number" -}
similarityFloor :: Int
similarityFloor = 7

{- Finds inexact matches for the input among the choices.
 - Returns an ordered list of good enough matches, or an empty list if
 - nothing matches well. -}
fuzzymatches :: String -> (c -> String) -> [c] -> [c]
fuzzymatches input showchoice choices = fst $ unzip $
	sortBy comparecost $ filter similarEnough $ zip choices costs
  where
	distance = restrictedDamerauLevenshteinDistance gitEditCosts input
	costs = map (distance . showchoice) choices
	comparecost a b = compare (snd a) (snd b)
	similarEnough (_, cst) = cst < similarityFloor

{- Takes action based on git's autocorrect configuration, in preparation for
 - an autocorrected command being run.
 -}
prepare :: String -> (c -> String) -> NE.NonEmpty c -> Maybe Repo -> IO ()
prepare input showmatch matches r =
	case readish . fromConfigValue . Git.Config.get "help.autocorrect" "0" =<< r of
		Just n
			| n == 0 -> list
			| n < 0 -> warn Nothing
			| otherwise -> sleep n
		Nothing -> list
  where
	list = giveup $ unlines $
		[ "Unknown command '" ++ input ++ "'"
		, ""
		, "Did you mean one of these?"
		] ++ map (\m -> "\t" ++ showmatch m) (NE.toList matches)
	warn :: Maybe Float -> IO ()
	warn mdelaysec = hPutStr stderr $ unlines
		[ "WARNING: You called a git-annex command named '" ++
		  input ++ "', which does not exist."
		, case mdelaysec of
			Nothing -> "Continuing under the assumption that you meant " ++ match
			Just sec -> "Continuing in " ++ show sec ++ " seconds, assuming that you meant " ++ match
		]
	  where
		match = "'" ++ showmatch (NE.head matches) ++ "'."
	sleep n = do
		warn (Just (fromIntegral n / 10 :: Float))
		threadDelay (n * 100000) -- deciseconds to microseconds
