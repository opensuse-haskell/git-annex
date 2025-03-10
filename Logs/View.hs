{- git-annex recent views log
 -
 - The most recently accessed view comes first.
 -
 - This file is stored locally in .git/annex/, not in the git-annex branch.
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.View (
	currentView,
	setView,
	removeView,
	recentViews,
	branchView,
	fromViewBranch,
	is_branchView,
	branchViewPrefix,
	prop_branchView_legal,
) where

import Annex.Common
import Types.View
import Types.MetaData
import Types.AdjustedBranch
import Annex.AdjustedBranch.Name
import qualified Git
import qualified Git.Branch
import qualified Git.Ref
import Git.Types
import Logs.File

import qualified Data.Text as T
import qualified Data.Set as S
import Data.Char
import qualified Data.ByteString as B

setView :: View -> Annex ()
setView v = do
	old <- take 99 . filter (/= v) <$> recentViews
	writeViews (v : old)

writeViews :: [View] -> Annex ()
writeViews l = do
	f <- fromRepo gitAnnexViewLog
	writeLogFile f $ unlines $ map show l

removeView :: View -> Annex ()
removeView v = writeViews =<< filter (/= v) <$> recentViews

recentViews :: Annex [View]
recentViews = do
	f <- fromOsPath <$> fromRepo gitAnnexViewLog
	liftIO $ mapMaybe readish . lines <$> catchDefaultIO [] (readFile f)

{- Gets the currently checked out view, if there is one. 
 -
 - The view may also have an adjustment applied to it.
 -}
currentView :: Annex (Maybe (View, Maybe Adjustment))
currentView = go =<< inRepo Git.Branch.current
  where
	go (Just b) = case adjustedToOriginal b of
		Nothing -> getvb b Nothing
		Just (adj, b') -> getvb b' (Just adj)
	go Nothing = return Nothing

	getvb b madj
		| branchViewPrefix `B.isPrefixOf` fromRef' b = do
			vb <- headMaybe 
				. filter (\v -> branchView v Nothing == b || branchViewOld v == b) 
				<$> recentViews
			case vb of
				Just vb' -> return (Just (vb', madj))
				Nothing -> return Nothing
		| otherwise = return Nothing

{- Note that this is not the prefix used when an adjustment is applied to a
 - view branch. -}
branchViewPrefix :: B.ByteString
branchViewPrefix = "refs/heads/views"

{- Generates a git branch name for a View, which may also have an
 - adjustment applied to it.
 - 
 - There is no guarantee that each view gets a unique branch name,
 - but the branch name is used to express the view as well as possible
 - given the constraints on git branch names. It includes the name of the
 - parent branch, and what metadata is used.
 -}
branchView :: View -> Maybe Adjustment -> Git.Branch
branchView view madj = case madj of
	Nothing -> vb
	Just adj -> adjBranch $ originalToAdjusted vb adj
  where
  	basebranch = fromRef' (Git.Ref.base (viewParentBranch view))
	vb = Git.Ref $ branchViewPrefix <> "/" <> basebranch
		<> "(" <> branchViewDesc view False <> ")"

{- Old name used for a view did not include the name of the parent branch. -}
branchViewOld :: View -> Git.Branch
branchViewOld view = Git.Ref $
	 branchViewPrefix <> "/" <> branchViewDesc view True

branchViewDesc :: View -> Bool -> B.ByteString
branchViewDesc view pareninvisibles = encodeBS $
	intercalate ";" $ map branchcomp (viewComponents view)
  where
	branchcomp c
		| viewVisible c || not pareninvisibles = branchcomp' c
		| otherwise = "(" ++ branchcomp' c ++ ")"
	branchcomp' (ViewComponent metafield viewfilter _) = concat
		[ forcelegal (T.unpack (fromMetaField metafield))
		, branchvals viewfilter
		]
	branchvals (FilterValues set) = '=' : branchset set
	branchvals (FilterGlob glob) = '=' : forcelegal glob
	branchvals (ExcludeValues set) = "!=" ++ branchset set
	branchvals (FilterValuesOrUnset set _) = '=' : branchset set
	branchvals (FilterGlobOrUnset glob _) = '=' : forcelegal glob
	branchset = intercalate ","
		. map (forcelegal . decodeBS . fromMetaValue)
		. S.toList
	forcelegal s
		| Git.Ref.legal True s = s
		| otherwise = map (\c -> if isAlphaNum c then c else '_') s

is_branchView :: Git.Branch -> Bool
is_branchView b = case adjustedToOriginal b of
	Nothing -> hasprefix b
	Just (_adj, b') -> hasprefix b'
  where
	hasprefix (Ref b') = (branchViewPrefix <> "/") `B.isPrefixOf` b'

{- Converts a view branch as generated by branchView (but not by
 - branchViewOld) back to the parent branch.
 - Has no effect on other branches. -}
fromViewBranch :: Git.Branch -> Git.Branch
fromViewBranch b = case adjustedToOriginal b of
	Nothing -> go b
	Just (_adj, b') -> go b'
  where
	go b' =
		let bs = fromRef' b'
		in if (branchViewPrefix <> "/") `B.isPrefixOf` bs
			then 
				let (branch, _desc) = separate' (== openparen) (B.drop prefixlen bs)
				in Ref branch
			else b'
	
	prefixlen = B.length branchViewPrefix + 1
	openparen = fromIntegral (ord '(')

prop_branchView_legal :: View -> Bool
prop_branchView_legal = Git.Ref.legal False 
	. fromRef . (\v -> branchView v Nothing)
