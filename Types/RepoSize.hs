{- git-annex repo sizes types
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.RepoSize where

-- The current size of a repo.
newtype RepoSize = RepoSize Integer
	deriving (Show, Eq, Ord)

-- The maximum size of a repo.
newtype MaxSize = MaxSize Integer
	deriving (Show, Eq, Ord)