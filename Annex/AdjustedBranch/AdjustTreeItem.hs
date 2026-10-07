{- adjusted branch
 -
 - Copyright 2016-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Annex.AdjustedBranch.AdjustTreeItem where

import Annex.Common
import Types.AdjustedBranch
import Git
import Git.Types
import Git.Tree (TreeItem(..))
import Git.FilePath
import Annex.CatFile
import Annex.Link
import Annex.Content.Presence
import qualified Database.Keys
import Utility.FileMode
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (fileMode)

class AdjustTreeItem t where
	-- How to perform various adjustments to a TreeItem.
	adjustTreeItem :: t -> TreeItem -> Annex (Maybe TreeItem)
	-- Will adjusting a given tree always yield the same adjusted tree?
	adjustmentIsStable :: t -> Bool

instance AdjustTreeItem Adjustment where
	adjustTreeItem (LinkAdjustment l) t = adjustTreeItem l t
	adjustTreeItem (PresenceAdjustment p Nothing) t = adjustTreeItem p t
	adjustTreeItem (PresenceAdjustment p (Just l)) t =
		adjustTreeItem p t >>= \case
			Nothing -> return Nothing
			Just t' -> adjustTreeItem l t'
	adjustTreeItem (LockUnlockPresentAdjustment l) t = adjustTreeItem l t

	adjustmentIsStable (LinkAdjustment l) = adjustmentIsStable l
	adjustmentIsStable (PresenceAdjustment p _) = adjustmentIsStable p
	adjustmentIsStable (LockUnlockPresentAdjustment l) = adjustmentIsStable l

instance AdjustTreeItem LinkAdjustment where
	adjustTreeItem UnlockAdjustment =
		ifSymlink adjustToPointer noAdjust
	adjustTreeItem LockAdjustment =
		ifSymlink noAdjust adjustToSymlink
	adjustTreeItem FixAdjustment =
		ifSymlink adjustToSymlink noAdjust
	adjustTreeItem UnFixAdjustment =
		ifSymlink (adjustToSymlink' gitAnnexLinkCanonical) noAdjust
	
	adjustmentIsStable _ = True

instance AdjustTreeItem PresenceAdjustment where
	adjustTreeItem HideMissingAdjustment = 
		ifPresent noAdjust hideAdjust
	adjustTreeItem ShowMissingAdjustment =
		noAdjust

	adjustmentIsStable HideMissingAdjustment = False
	adjustmentIsStable ShowMissingAdjustment = True

instance AdjustTreeItem LockUnlockPresentAdjustment where
	adjustTreeItem UnlockPresentAdjustment = 
		ifPresent adjustToPointer adjustToSymlink
	adjustTreeItem LockPresentAdjustment =
		-- Turn all pointers back to symlinks, whether the content
		-- is present or not. This is done because the content
		-- availability may have changed and the branch not been
		-- re-adjusted to keep up, so there may be pointers whose
		-- content is not present.
		ifSymlink noAdjust adjustToSymlink

	adjustmentIsStable UnlockPresentAdjustment = False
	adjustmentIsStable LockPresentAdjustment = True

ifSymlink
	:: (TreeItem -> Annex a)
	-> (TreeItem -> Annex a)
	-> TreeItem
	-> Annex a
ifSymlink issymlink notsymlink ti@(TreeItem _f m _s)
	| toTreeItemType m == Just TreeSymlink = issymlink ti
	| otherwise = notsymlink ti

ifPresent
	:: (TreeItem -> Annex (Maybe TreeItem))
	-> (TreeItem -> Annex (Maybe TreeItem))
	-> TreeItem
	-> Annex (Maybe TreeItem)
ifPresent ispresent notpresent ti@(TreeItem _ _ s) =
	catKey s >>= \case
		Just k -> ifM (inAnnex k) (ispresent ti, notpresent ti)
		Nothing -> return (Just ti)

noAdjust :: TreeItem -> Annex (Maybe TreeItem)
noAdjust = return . Just

hideAdjust :: TreeItem -> Annex (Maybe TreeItem)
hideAdjust _ = return Nothing

adjustToPointer :: TreeItem -> Annex (Maybe TreeItem)
adjustToPointer ti@(TreeItem f _m s) = catKey s >>= \case
	Just k -> do
		Database.Keys.addAssociatedFile k f
		exe <- catchDefaultIO False $
			(isExecutable . fileMode) <$> 
				(liftIO . R.getFileStatus . fromOsPath
					=<< calcRepo (gitAnnexLocation k))
		let mode = fromTreeItemType $ 
			if exe then TreeExecutable else TreeFile
		Just . TreeItem f mode <$> hashPointerFile k
	Nothing -> return (Just ti)

adjustToSymlink :: TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink = adjustToSymlink' gitAnnexLink

adjustToSymlink' :: (OsPath -> Key -> Git.Repo -> GitConfig -> IO OsPath) -> TreeItem -> Annex (Maybe TreeItem)
adjustToSymlink' gitannexlink ti@(TreeItem f _m s) = catKey s >>= \case
	Just k -> do
		absf <- inRepo $ \r -> absPath $ fromTopFilePath f r
		linktarget <- calcRepo $ gitannexlink absf k
		Just . TreeItem f (fromTreeItemType TreeSymlink)
			<$> hashSymlink (fromOsPath linktarget)
	Nothing -> return (Just ti)
