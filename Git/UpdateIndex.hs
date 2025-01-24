{- git-update-index library
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, OverloadedStrings, CPP #-}

module Git.UpdateIndex (
	Streamer,
	pureStreamer,
	streamUpdateIndex,
	streamUpdateIndex',
	withUpdateIndex,
	lsTree,
	lsSubTree,
	updateIndexLine,
	stageFile,
	unstageFile,
	stageSymlink,
	stageDiffTreeItem,
	refreshIndex,
) where

import Common
import Git
import Git.Types
import Git.Command
import Git.FilePath
import Git.Sha
import qualified Git.DiffTreeItem as Diff

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class

{- Streamers are passed a callback and should feed it lines in the form
 - read by update-index, and generated by ls-tree. -}
type Streamer = (L.ByteString -> IO ()) -> IO ()

{- A streamer with a precalculated value. -}
pureStreamer :: L.ByteString -> Streamer
pureStreamer !s = \streamer -> streamer s

{- Streams content into update-index from a list of Streamers. -}
streamUpdateIndex :: Repo -> [Streamer] -> IO ()
streamUpdateIndex repo as = withUpdateIndex repo $ \h ->
	forM_ as $ streamUpdateIndex' h

data UpdateIndexHandle = UpdateIndexHandle Handle

streamUpdateIndex' :: UpdateIndexHandle -> Streamer -> IO ()
streamUpdateIndex' (UpdateIndexHandle h) a = a $ \s -> do
	L.hPutStr h s
	L.hPutStr h "\0"

withUpdateIndex :: (MonadIO m, MonadMask m) => Repo -> (UpdateIndexHandle -> m a) -> m a
withUpdateIndex repo a = bracket setup cleanup go
  where
	params = map Param ["update-index", "-z", "--index-info"]
	
	setup = liftIO $ createProcess $ 
		(gitCreateProcess params repo)
			{ std_in = CreatePipe }
	go p = do
		r <- a (UpdateIndexHandle (stdinHandle p))
		liftIO $ do
			hClose (stdinHandle p)
			void $ checkSuccessProcess (processHandle p)
		return r
	
	cleanup = liftIO . cleanupProcess

{- A streamer that adds the current tree for a ref. Useful for eg, copying
 - and modifying branches. -}
lsTree :: Ref -> Repo -> Streamer
lsTree (Ref x) repo streamer = do
	(s, cleanup) <- pipeNullSplit params repo
	mapM_ streamer s
	void $ cleanup
  where
	params = map Param ["ls-tree", "-z", "-r", "--full-tree", decodeBS x]
lsSubTree :: Ref -> FilePath -> Repo -> Streamer
lsSubTree (Ref x) p repo streamer = do
	(s, cleanup) <- pipeNullSplit params repo
	mapM_ streamer s
	void $ cleanup
  where
	params = map Param ["ls-tree", "-z", "-r", "--full-tree", decodeBS x, p]

{- Generates a line suitable to be fed into update-index, to add
 - a given file with a given sha. -}
updateIndexLine :: Sha -> TreeItemType -> TopFilePath -> L.ByteString
updateIndexLine sha treeitemtype file = L.fromStrict $
	fmtTreeItemType treeitemtype
	<> " blob "
	<> fromRef' sha
	<> "\t"
	<> fromOsPath (indexPath file)

stageFile :: Sha -> TreeItemType -> OsPath -> Repo -> IO Streamer
stageFile sha treeitemtype file repo = do
	p <- toTopFilePath file repo
	return $ pureStreamer $ updateIndexLine sha treeitemtype p

{- A streamer that removes a file from the index. -}
unstageFile :: OsPath -> Repo -> IO Streamer
unstageFile file repo = do
	p <- toTopFilePath file repo
	return $ unstageFile' p

unstageFile' :: TopFilePath -> Streamer
unstageFile' p = pureStreamer $ L.fromStrict $
	"0 "
	<> fromRef' deleteSha
	<> "\t"
	<> fromOsPath (indexPath p)

{- A streamer that adds a symlink to the index. -}
stageSymlink :: OsPath -> Sha -> Repo -> IO Streamer
stageSymlink file sha repo = do
	!line <- updateIndexLine
		<$> pure sha
		<*> pure TreeSymlink
		<*> toTopFilePath file repo
	return $ pureStreamer line

{- A streamer that applies a DiffTreeItem to the index. -}
stageDiffTreeItem :: Diff.DiffTreeItem -> Streamer
stageDiffTreeItem d = case toTreeItemType (Diff.dstmode d) of
	Nothing -> unstageFile' (Diff.file d)
	Just t -> pureStreamer $ updateIndexLine (Diff.dstsha d) t (Diff.file d)

indexPath :: TopFilePath -> InternalGitPath
indexPath = toInternalGitPath . getTopFilePath

{- Refreshes the index, by checking file stat information.
 -
 - The action is passed a callback that it can use to send filenames to
 - update-index. Sending Nothing will wait for update-index to finish
 - updating the index.
 -}
refreshIndex :: (MonadIO m, MonadMask m) => Repo -> ((Maybe OsPath -> IO ()) -> m ()) -> m ()
refreshIndex repo feeder = bracket
	(liftIO $ createProcess p)
	(liftIO . cleanupProcess)
	go
  where
	params = 
		[ Param "update-index"
		, Param "-q"
		, Param "--refresh"
		, Param "-z"
		, Param "--stdin"
		]
	
	p = (gitCreateProcess params repo)
		{ std_in = CreatePipe }

	go (Just h, _, _, pid) = do
		let closer = do
			hClose h
			forceSuccessProcess p pid
		feeder $ \case
			Just f -> S.hPut h (S.snoc (fromOsPath f) 0)
			Nothing -> closer
		liftIO $ closer
	go _ = error "internal"
