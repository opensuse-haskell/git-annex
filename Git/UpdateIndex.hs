{- git-update-index library
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, CPP #-}

module Git.UpdateIndex (
	Streamer,
	pureStreamer,
	streamUpdateIndex,
	streamUpdateIndex',
	startUpdateIndex,
	stopUpdateIndex,
	lsTree,
	updateIndexLine,
	stageFile,
	unstageFile,
	stageSymlink
) where

import Common
import Git
import Git.Types
import Git.Command
import Git.FilePath
import Git.Sha

import Control.Exception (bracket)
import System.Process (std_in)

{- Streamers are passed a callback and should feed it lines in the form
 - read by update-index, and generated by ls-tree. -}
type Streamer = (String -> IO ()) -> IO ()

{- A streamer with a precalculated value. -}
pureStreamer :: String -> Streamer
pureStreamer !s = \streamer -> streamer s

{- Streams content into update-index from a list of Streamers. -}
streamUpdateIndex :: Repo -> [Streamer] -> IO ()
streamUpdateIndex repo as = bracket (startUpdateIndex repo) stopUpdateIndex $
	(\h -> forM_ as $ streamUpdateIndex' h)

data UpdateIndexHandle = UpdateIndexHandle ProcessHandle Handle

streamUpdateIndex' :: UpdateIndexHandle -> Streamer -> IO ()
streamUpdateIndex' (UpdateIndexHandle _ h) a = a $ \s -> do
	hPutStr h s
	hPutStr h "\0"

startUpdateIndex :: Repo -> IO UpdateIndexHandle
startUpdateIndex repo = do
	(Just h, _, _, p) <- createProcess (gitCreateProcess params repo)
		{ std_in = CreatePipe }
	fileEncoding h
	return $ UpdateIndexHandle p h
  where
	params = map Param ["update-index", "-z", "--index-info"]

stopUpdateIndex :: UpdateIndexHandle -> IO Bool
stopUpdateIndex (UpdateIndexHandle p h) = do
	hClose h
	checkSuccessProcess p

{- A streamer that adds the current tree for a ref. Useful for eg, copying
 - and modifying branches. -}
lsTree :: Ref -> Repo -> Streamer
lsTree (Ref x) repo streamer = do
	(s, cleanup) <- pipeNullSplit params repo
	mapM_ streamer s
	void $ cleanup
  where
	params = map Param ["ls-tree", "-z", "-r", "--full-tree", x]

{- Generates a line suitable to be fed into update-index, to add
 - a given file with a given sha. -}
updateIndexLine :: Sha -> BlobType -> TopFilePath -> String
updateIndexLine sha filetype file =
	show filetype ++ " blob " ++ show sha ++ "\t" ++ indexPath file

stageFile :: Sha -> BlobType -> FilePath -> Repo -> IO Streamer
stageFile sha filetype file repo = do
	p <- toTopFilePath file repo
	return $ pureStreamer $ updateIndexLine sha filetype p

{- A streamer that removes a file from the index. -}
unstageFile :: FilePath -> Repo -> IO Streamer
unstageFile file repo = do
	p <- toTopFilePath file repo
	return $ pureStreamer $ "0 " ++ show nullSha ++ "\t" ++ indexPath p

{- A streamer that adds a symlink to the index. -}
stageSymlink :: FilePath -> Sha -> Repo -> IO Streamer
stageSymlink file sha repo = do
	!line <- updateIndexLine
		<$> pure sha
		<*> pure SymlinkBlob
		<*> toTopFilePath file repo
	return $ pureStreamer line

indexPath :: TopFilePath -> InternalGitPath
indexPath = toInternalGitPath . getTopFilePath
