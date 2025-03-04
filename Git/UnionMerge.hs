{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.UnionMerge (
	merge,
	mergeIndex
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.Set as S

import Common
import Git
import Git.Sha
import Git.CatFile
import Git.Command
import Git.UpdateIndex
import Git.HashObject
import Git.Types
import Git.FilePath

{- Performs a union merge between two branches, staging it in the index.
 - Any previously staged changes in the index will be lost.
 -
 - Should be run with a temporary index file configured by useIndex.
 -}
merge :: Ref -> Ref -> Repo -> IO ()
merge x y repo = do
	hashhandle <- hashObjectStart True repo
	ch <- catFileStart repo
	streamUpdateIndex repo
		[ lsTree x repo
		, mergeTrees x y hashhandle ch repo
		]
	catFileStop ch
	hashObjectStop hashhandle

{- Merges a list of branches into the index. Previously staged changes in
 - the index are preserved (and participate in the merge).
 -
 - update-index is run once per ref in turn, so that each ref is merged on
 - top of the merge for the previous ref. It would be more efficient, but
 - harder to calculate a single union merge involving all the refs, as well
 - as the index.
 -}
mergeIndex :: HashObjectHandle -> CatFileHandle -> Repo -> [Ref] -> IO ()
mergeIndex hashhandle ch repo bs = forM_ bs $ \b ->
	streamUpdateIndex repo [mergeTreeIndex b hashhandle ch repo]

{- For merging two trees. -}
mergeTrees :: Ref -> Ref -> HashObjectHandle -> CatFileHandle -> Repo -> Streamer
mergeTrees x y hashhandle ch = doMerge hashhandle ch
	("diff-tree":diffOpts ++ [fromRef x, fromRef y, "--"])

{- For merging a single tree into the index. -}
mergeTreeIndex :: Ref -> HashObjectHandle -> CatFileHandle -> Repo -> Streamer
mergeTreeIndex r hashhandle ch = doMerge hashhandle ch $
	"diff-index" : diffOpts ++ ["--cached", fromRef r, "--"]

diffOpts :: [String]
diffOpts = ["--raw", "-z", "-r", "--no-renames", "-l0"]

{- Streams update-index changes to perform a merge,
 - using git to get a raw diff. -}
doMerge :: HashObjectHandle -> CatFileHandle -> [String] -> Repo -> Streamer
doMerge hashhandle ch differ repo streamer = do
	(diff, cleanup) <- pipeNullSplit' (map Param differ) repo
	go diff
	void $ cleanup
  where
	go [] = noop
	go (info:file:rest) = mergeFile info (toOsPath file) hashhandle ch >>=
		maybe (go rest) (\l -> streamer l >> go rest)
	go (_:[]) = giveup $ "parse error " ++ show differ

{- Given an info line from a git raw diff, and the filename, generates
 - a line suitable for update-index that union merges the two sides of the
 - diff. -}
mergeFile :: S.ByteString -> OsPath -> HashObjectHandle -> CatFileHandle -> IO (Maybe L.ByteString)
mergeFile info file hashhandle h = case S8.words info of
	[_colonmode, _bmode, asha, bsha, _status] -> 
		case filter (`notElem` nullShas) [Ref asha, Ref bsha] of
			[] -> return Nothing
			(sha:[]) -> use sha
			shas -> use
				=<< either return (hashBlob hashhandle . L8.unlines)
				=<< calcMerge . zip shas <$> mapM getcontents shas
	_ -> return Nothing
  where
	use sha = return $ Just $
		updateIndexLine sha TreeFile $ asTopFilePath file
	-- Get file and split into lines to union merge.
	-- The encoding of the file is assumed to be either ASCII or utf-8;
	-- in either case it's safe to split on \n
	getcontents s = L8.lines <$> catObject h s

{- Calculates a union merge between a list of refs, with contents.
 -
 - When possible, reuses the content of an existing ref, rather than
 - generating new content.
 -}
calcMerge :: [(Ref, [L8.ByteString])] -> Either Ref [L8.ByteString]
calcMerge shacontents = case reusable of
	[] -> Right new
	(r:_) -> Left $ fst r
  where
	reusable = filter (\c -> sorteduniq (snd c) == new) shacontents
	new = sorteduniq $ concat $ map snd shacontents
	sorteduniq = S.toList . S.fromList
