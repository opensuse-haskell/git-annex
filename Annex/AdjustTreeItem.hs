{- TreeItem adjustment implementation
 -
 - This is separate from Annex.AdjustedBranch to allow it to import modules
 - that depend on Annex.AdjustedBranch.
 -
 - Copyright 2016-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Annex.AdjustTreeItem (
	AdjustTreeItem,
	getAdjustTreeItem
) where

import Annex.Common
import Types.AdjustedBranch
import Types.AdjustTreeItem
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

getAdjustTreeItem :: AdjustTreeItem
getAdjustTreeItem = AdjustTreeItem
	{ adjustTreeItem = adjustTreeItemC
	, adjustmentIsStable = adjustmentIsStableC
	}

class AdjustTreeItemClass t where
	-- How to perform various adjustments to a TreeItem.
	adjustTreeItemC :: t -> TreeItem -> Annex (Maybe TreeItem)
	-- Will adjusting a given tree always yield the same adjusted tree?
	adjustmentIsStableC :: t -> Bool

instance AdjustTreeItemClass Adjustment where
	adjustTreeItemC (LinkAdjustment l) t = adjustTreeItemC l t
	adjustTreeItemC (PresenceAdjustment p Nothing) t = adjustTreeItemC p t
	adjustTreeItemC (PresenceAdjustment p (Just l)) t =
		adjustTreeItemC p t >>= \case
			Nothing -> return Nothing
			Just t' -> adjustTreeItemC l t'
	adjustTreeItemC (LockUnlockPresentAdjustment l) t = adjustTreeItemC l t

	adjustmentIsStableC (LinkAdjustment l) = adjustmentIsStableC l
	adjustmentIsStableC (PresenceAdjustment p _) = adjustmentIsStableC p
	adjustmentIsStableC (LockUnlockPresentAdjustment l) = adjustmentIsStableC l

instance AdjustTreeItemClass LinkAdjustment where
	adjustTreeItemC UnlockAdjustment =
		ifSymlink adjustToPointer noAdjust
	adjustTreeItemC LockAdjustment =
		ifSymlink noAdjust adjustToSymlink
	adjustTreeItemC FixAdjustment =
		ifSymlink adjustToSymlink noAdjust
	adjustTreeItemC UnFixAdjustment =
		ifSymlink (adjustToSymlink' gitAnnexLinkCanonical) noAdjust
	
	adjustmentIsStableC _ = True

instance AdjustTreeItemClass PresenceAdjustment where
	adjustTreeItemC HideMissingAdjustment = 
		ifPresent noAdjust hideAdjust
	adjustTreeItemC ShowMissingAdjustment =
		noAdjust

	adjustmentIsStableC HideMissingAdjustment = False
	adjustmentIsStableC ShowMissingAdjustment = True

instance AdjustTreeItemClass LockUnlockPresentAdjustment where
	adjustTreeItemC UnlockPresentAdjustment = 
		ifPresent adjustToPointer adjustToSymlink
	adjustTreeItemC LockPresentAdjustment =
		-- Turn all pointers back to symlinks, whether the content
		-- is present or not. This is done because the content
		-- availability may have changed and the branch not been
		-- re-adjusted to keep up, so there may be pointers whose
		-- content is not present.
		ifSymlink noAdjust adjustToSymlink

	adjustmentIsStableC UnlockPresentAdjustment = False
	adjustmentIsStableC LockPresentAdjustment = True

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
