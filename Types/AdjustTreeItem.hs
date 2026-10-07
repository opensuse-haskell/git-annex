{- type for adjusting a TreeItem
 -
 - Copyright 2016-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.AdjustTreeItem where

import Annex.Common
import Types.AdjustedBranch
import Git.Tree (TreeItem(..))

data AdjustTreeItem = AdjustTreeItem
	-- How to perform various adjustments to a TreeItem.
	{ adjustTreeItem :: Adjustment -> TreeItem -> Annex (Maybe TreeItem)
	-- Will adjusting a given tree always yield the same adjusted tree?
	, adjustmentIsStable :: Adjustment -> Bool
	}
