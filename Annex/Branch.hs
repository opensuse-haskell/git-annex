{- management of the git-annex branch
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Branch (
	fullname,
	name,
	hasOrigin,
	hasSibling,
	siblingBranches,
	create,
	getBranch,
	UpdateMade(..),
	update,
	forceUpdate,
	updateTo,
	get,
	getHistorical,
	getRef,
	getUnmergedRefs,
	RegardingUUID(..),
	change,
	ChangeOrAppend(..),
	changeOrAppend,
	maybeChange,
	commitMessage,
	createMessage,
	commit,
	forceCommit,
	files,
	rememberTreeish,
	performTransitions,
	withIndex,
	precache,
	UnmergedBranches(..),
	FileContents,
	overBranchFileContents,
	overJournalFileContents,
	combineStaleJournalWithBranch,
	updatedFromTree,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.Char
import Data.ByteString.Builder
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import System.PosixCompat.Files (isRegularFile)

import Annex.Common
import Types.BranchState
import Annex.BranchState
import Annex.Journal
import Annex.GitOverlay
import Annex.Tmp
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.RefLog
import qualified Git.Sha
import qualified Git.Branch
import qualified Git.UnionMerge
import qualified Git.UpdateIndex
import qualified Git.Tree
import qualified Git.LsTree
import Git.LsTree (lsTreeParams)
import qualified Git.HashObject
import Annex.HashObject
import Git.Types (Ref(..), fromRef, fromRef', RefDate, TreeItemType(..))
import Git.FilePath
import Annex.CatFile
import Git.CatFile (catObjectStreamLsTree)
import Annex.Perms
import Logs
import Logs.Transitions
import Logs.File
import Logs.Trust.Pure
import Logs.Remote.Pure
import Logs.Export.Pure
import Logs.Difference.Pure
import qualified Annex.Queue
import Types.Transitions
import Annex.Branch.Transitions
import qualified Annex
import Annex.Hook
import Utility.Directory.Stream
import Utility.Tmp
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F

{- Name of the branch that is used to store git-annex's information. -}
name :: Git.Ref
name = Git.Ref "git-annex"

{- Fully qualified name of the branch. -}
fullname :: Git.Ref
fullname = Git.Ref $ "refs/heads/" <> fromRef' name

{- Branch's name in origin. -}
originname :: Git.Ref
originname = Git.Ref $ "refs/remotes/origin/" <> fromRef' name

{- Does origin/git-annex exist? -}
hasOrigin :: Annex Bool
hasOrigin = inRepo $ Git.Ref.exists originname

{- Does the git-annex branch or a sibling foo/git-annex branch exist? -}
hasSibling :: Annex Bool
hasSibling = not . null <$> siblingBranches

{- List of git-annex (shas, branches), including the main one and any
 - from remotes. Duplicates are filtered out. -}
siblingBranches :: Annex [(Git.Sha, Git.Branch)]
siblingBranches = inRepo $ Git.Ref.matchingUniq [name]

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = void getBranch

{- Returns the sha of the branch, creating it first if necessary. -}
getBranch :: Annex Git.Sha
getBranch = maybe (hasOrigin >>= go >>= use) return =<< branchsha
  where
	go True = do
		inRepo $ Git.Command.run
			[ Param "branch"
			, Param "--no-track"
			, Param $ fromRef name
			, Param $ fromRef originname
			]
		fromMaybe (giveup $ "failed to create " ++ fromRef name)
			<$> branchsha
	go False = withIndex' True $ do
		-- Create the index file. This is not necessary,
		-- except to avoid a bug in git 2.37 that causes
		-- git write-tree to segfault when the index file does not
		-- exist.
		inRepo $ flip Git.UpdateIndex.streamUpdateIndex []
		cmode <- annexCommitMode <$> Annex.getGitConfig
		cmessage <- createMessage
		inRepo $ Git.Branch.commitAlways cmode cmessage fullname []
	use sha = do
		setIndexSha sha
		return sha
	branchsha = inRepo $ Git.Ref.sha fullname

{- Ensures that the branch and index are up-to-date; should be
 - called before data is read from it. Runs only once per git-annex run. -}
update :: Annex BranchState
update = runUpdateOnce $ updateTo =<< siblingBranches

{- Forces an update even if one has already been run. -}
forceUpdate :: Annex UpdateMade
forceUpdate = updateTo =<< siblingBranches

{- Merges the specified Refs into the index, if they have any changes not
 - already in it. The Branch names are only used in the commit message;
 - it's even possible that the provided Branches have not been updated to
 - point to the Refs yet.
 - 
 - The branch is fast-forwarded if possible, otherwise a merge commit is
 - made.
 -
 - Before Refs are merged into the index, it's important to first stage the
 - journal into the index. Otherwise, any changes in the journal would
 - later get staged, and might overwrite changes made during the merge.
 - This is only done if some of the Refs do need to be merged.
 -
 - Also handles performing any Transitions that have not yet been
 - performed, in either the local branch, or the Refs.
 -}
updateTo :: [(Git.Sha, Git.Branch)] -> Annex UpdateMade
updateTo pairs = ifM (annexMergeAnnexBranches <$> Annex.getGitConfig)
	( updateTo' pairs
	, return (UpdateMade False False)
	)

updateTo' :: [(Git.Sha, Git.Branch)] -> Annex UpdateMade
updateTo' pairs = do
	-- ensure branch exists, and get its current ref
	branchref <- getBranch
	ignoredrefs <- getIgnoredRefs
	let unignoredrefs = excludeset ignoredrefs pairs
	(tomerge, notnewer) <- if null unignoredrefs
		then return ([], [])
		else do
			mergedrefs <- getMergedRefs
			partitionM isnewer $
				excludeset mergedrefs unignoredrefs
	{- In a read-only repository, catching permission denied lets
	 - query operations still work, although they will need to do
	 - additional work since the refs are not merged. -}
	catchPermissionDenied
		(const (updatefailedperms tomerge))
		(go branchref tomerge notnewer)
  where
	excludeset s = filter (\(r, _) -> S.notMember r s)

	isnewer (r, _) = inRepo $ Git.Branch.changed fullname r

	go branchref tomerge notnewer = do
		dirty <- journalDirty gitAnnexJournalDir
		journalcleaned <- if null tomerge
			{- Even when no refs need to be merged, the index
			 - may still be updated if the branch has gotten ahead 
			 - of the index, or just if the journal is dirty. -}
			then ifM (needUpdateIndex branchref)
				( lockJournal $ \jl -> do
					forceUpdateIndex jl branchref
					{- When there are journalled changes
					 - as well as the branch being updated,
					 - a commit needs to be done. -}
					when dirty $
						go' branchref dirty [] jl
					return True
				, if dirty
					then ifM (annexAlwaysCommit <$> Annex.getGitConfig)
						( lockJournal $ \jl -> do
							go' branchref dirty [] jl
							return True
						, return False
						)
					else return True
				)
			else lockJournal $ \jl -> do
				go' branchref dirty tomerge jl
				return True
		journalclean <- if journalcleaned
			then not <$> privateUUIDsKnown
			else pure False
		addMergedRefs notnewer
		return $ UpdateMade
			{ refsWereMerged = not (null tomerge)
			, journalClean = journalclean 
			}
	
	go' branchref dirty tomerge jl = stagejournalwhen dirty jl $ do
		let (refs, branches) = unzip tomerge
		merge_desc <- if null tomerge
			then commitMessage
			else return $ "merging " ++
				unwords (map Git.Ref.describe branches) ++ 
				" into " ++ fromRef name
		localtransitions <- getLocalTransitions
		unless (null tomerge) $ do
			showSideAction (UnquotedString merge_desc)
			mapM_ checkBranchDifferences refs
			mergeIndex jl refs
		let commitrefs = nub $ fullname:refs
		ifM (handleTransitions jl localtransitions commitrefs)
			( runAnnexHook postUpdateAnnexHook annexPostUpdateCommand
			, do
				ff <- if dirty
					then return False
					else inRepo $ Git.Branch.fastForward fullname refs
				if ff
					then updateIndex jl branchref
					else commitIndex jl branchref merge_desc commitrefs
			)
		addMergedRefs tomerge
		invalidateCacheAll
	
	stagejournalwhen dirty jl a
		| dirty = stageJournal jl a
		| otherwise = withIndex a
	
	-- Preparing for read-only branch access with unmerged remote refs.
	updatefailedperms tomerge = do
		let refs = map fst tomerge
		-- Gather any transitions that are new to either the
		-- local branch or a remote ref, which will need to be
		-- applied on the fly.
		localts <- getLocalTransitions
		remotets <- mapM getRefTransitions refs
		ts <- if all (localts ==) remotets
			then return []
			else 
				let tcs = mapMaybe getTransitionCalculator $
					knownTransitionList $
						combineTransitions (localts:remotets)
				in if null tcs
					then return []
					else do
						config <- Annex.getGitConfig
						trustmap <- calcTrustMap <$> getStaged trustLog
						remoteconfigmap <- calcRemoteConfigMap <$> getStaged remoteLog
						return $ map (\c -> c trustmap remoteconfigmap config) tcs
		return $ UpdateFailedPermissions 
			{ refsUnmerged = refs
			, newTransitions = ts
			}

{- Gets the content of a file, which may be in the journal, or in the index
 - (and committed to the branch). 
 -
 - Returns an empty string if the file doesn't exist yet.
 - 
 - Updates the branch if necessary, to ensure the most up-to-date available
 - content is returned. 
 -
 - When permissions prevented updating the branch, reads the content from the
 - journal, plus the branch, plus all unmerged refs. In this case, any
 - transitions that have not been applied to all refs will be applied on
 - the fly.
 -}
get :: OsPath -> Annex L.ByteString
get file = do
	st <- update
	case getCache file st of
		Just content -> return content
		Nothing -> do
			content <- if journalIgnorable st
				then getRef fullname file
				else if null (unmergedRefs st) 
					then getLocal file
					else unmergedbranchfallback st
			setCache file content
			return content
  where
	unmergedbranchfallback st = do
		l <- getLocal file
		bs <- forM (unmergedRefs st) $ \ref -> getRef ref file
		let content = l <> mconcat bs
		return $ applytransitions (unhandledTransitions st) content
	applytransitions [] content = content
	applytransitions (changer:rest) content = case changer file content of
		PreserveFile -> applytransitions rest content
		ChangeFile builder -> do
			let content' = toLazyByteString builder
			if L.null content'
				-- File is deleted, can't run any other
				-- transitions on it.
				then content'
				else applytransitions rest content'

{- When the git-annex branch is unable to be updated due to permissions,
 - and there are other git-annex branches that have not been merged into
 - it, this gets the refs of those branches. -}
getUnmergedRefs :: Annex [Git.Ref]
getUnmergedRefs = unmergedRefs <$> update

{- Used to cache the value of a file, which has been read from the branch
 - using some optimised method. The journal has to be checked, in case
 - it has a newer version of the file that has not reached the branch yet.
 -}
precache :: OsPath -> L.ByteString -> Annex ()
precache file branchcontent = do
	st <- getState
	content <- if journalIgnorable st
		then pure branchcontent
		else getJournalFileStale (GetPrivate True) file >>= return . \case
			NoJournalledContent -> branchcontent
			JournalledContent journalcontent -> journalcontent
			PossiblyStaleJournalledContent journalcontent ->
				branchcontent <> journalcontent
	setCache file content

{- Like get, but does not merge the branch, so the info returned may not
 - reflect changes in remotes.
 - (Changing the value this returns, and then merging is always the
 - same as using get, and then changing its value.) -}
getLocal :: OsPath -> Annex L.ByteString
getLocal = getLocal' (GetPrivate True)

getLocal' :: GetPrivate -> OsPath -> Annex L.ByteString
getLocal' getprivate file = do
	fastDebug "Annex.Branch" ("read " ++ fromOsPath file)
	go =<< getJournalFileStale getprivate file
  where
	go NoJournalledContent = getRef fullname file
	go (JournalledContent journalcontent) = return journalcontent
	go (PossiblyStaleJournalledContent journalcontent) = do
		v <- getRef fullname file
		return (v <> journalcontent)

{- Gets the content of a file as staged in the branch's index. -}
getStaged :: OsPath -> Annex L.ByteString
getStaged = getRef indexref
  where
	-- This makes git cat-file be run with ":file",
	-- so it looks at the index.
	indexref = Ref ""

getHistorical :: RefDate -> OsPath -> Annex L.ByteString
getHistorical date file =
	-- This check avoids some ugly error messages when the reflog
	-- is empty.
	ifM (null <$> inRepo (Git.RefLog.get' [Param (fromRef fullname), Param "-n1"]))
		( giveup ("No reflog for " ++ fromRef fullname)
		, getRef (Git.Ref.dateRef fullname date) file
		)

getRef :: Ref -> OsPath -> Annex L.ByteString
getRef ref file = withIndex $ catFile ref file

{- Applies a function to modify the content of a file.
 -
 - Note that this does not cause the branch to be merged, it only
 - modifies the current content of the file on the branch.
 -}
change :: Journalable content => RegardingUUID -> OsPath -> (L.ByteString -> content) -> Annex ()
change ru file f = lockJournal $ \jl -> f <$> getToChange ru file >>= set jl ru file

{- Applies a function which can modify the content of a file, or not.
 -
 - When the file was modified, runs the onchange action, and returns
 - True. The action is run while the journal is still locked,
 - so another concurrent call to this cannot happen while it is running. -}
maybeChange :: Journalable content => RegardingUUID -> OsPath -> (L.ByteString -> Maybe content) -> Annex () -> Annex Bool
maybeChange ru file f onchange = lockJournal $ \jl -> do
	v <- getToChange ru file
	case f v of
		Just jv ->
			let b = journalableByteString jv
			in if v /= b
				then do
					set jl ru file b
					onchange
					return True
				else return False
		_ -> return False

data ChangeOrAppend t = Change t | Append t

{- Applies a function that can either modify the content of the file,
 - or append to the file. Appending can be more efficient when several
 - lines are written to a file in succession.
 -
 - When annex.alwayscompact=false, the function is not passed the content
 - of the journal file when the journal file already exists, and whatever
 - value it provides is always appended to the journal file. That avoids
 - reading the journal file, and so can be faster when many lines are being
 - written to it. The information that is recorded will be effectively the
 - same, only obsolete log lines will not get compacted.
 -
 - Currently, only appends when annex.alwayscompact=false. That is to
 - avoid appending when an older version of git-annex is also in use in the
 - same repository. An interrupted append could leave the journal file in a
 - state that would confuse the older version. This is planned to be
 - changed in a future repository version.
 -}
changeOrAppend :: Journalable content => RegardingUUID -> OsPath -> (L.ByteString -> ChangeOrAppend content) -> Annex ()
changeOrAppend ru file f = lockJournal $ \jl ->
	checkCanAppendJournalFile jl ru file >>= \case
		Just appendable -> ifM (annexAlwaysCompact <$> Annex.getGitConfig)
			( do
				oldc <- getToChange ru file
				case f oldc of
					Change newc -> set jl ru file newc
					Append toappend -> 
						set jl ru file $
							oldc <> journalableByteString toappend
						-- Use this instead in v11
						-- or whatever.
						-- append jl file appendable toappend
			, case f mempty of
				-- Append even though a change was
				-- requested; since mempty was passed in,
				-- the lines requested to change are
				-- minimized.
				Change newc -> append jl file appendable newc
				Append toappend -> append jl file appendable toappend
			)
		Nothing -> do
			oldc <- getToChange ru file
			case f oldc of
				Change newc -> set jl ru file newc
				-- Journal file does not exist yet, so
				-- cannot append and have to write it all.
				Append toappend -> set jl ru file $
					oldc <> journalableByteString toappend

{- Only get private information when the RegardingUUID is itself private. -}
getToChange :: RegardingUUID -> OsPath -> Annex L.ByteString
getToChange ru f = flip getLocal' f . GetPrivate =<< regardingPrivateUUID ru

{- Records new content of a file into the journal.
 -
 - This is not exported; all changes have to be made via change. This
 - ensures that information that was written to the branch is not
 - overwritten. Also, it avoids a get followed by a set without taking into
 - account whether private information was gotten from the private
 - git-annex index, and should not be written to the public git-annex
 - branch.
 -}
set :: Journalable content => JournalLocked -> RegardingUUID -> OsPath -> content -> Annex ()
set jl ru f c = do
	journalChanged
	setJournalFile jl ru f c
	fastDebug "Annex.Branch" ("set " ++ fromOsPath f)
	-- Could cache the new content, but it would involve
	-- evaluating a Journalable Builder twice, which is not very
	-- efficient. Instead, assume that it's not common to need to read
	-- a log file immediately after writing it.
	invalidateCache f

{- Appends content to the journal file. -}
append :: Journalable content => JournalLocked -> OsPath -> AppendableJournalFile -> content -> Annex ()
append jl f appendable toappend = do
	journalChanged
	appendJournalFile jl appendable toappend
	fastDebug "Annex.Branch" ("append " ++ fromOsPath f)
	invalidateCache f

{- Commit message used when making a commit of whatever data has changed
 - to the git-annex branch. -}
commitMessage :: Annex String
commitMessage = fromMaybe "update" <$> getCommitMessage

{- Commit message used when creating the branch. -}
createMessage :: Annex String
createMessage = fromMaybe "branch created" <$> getCommitMessage

getCommitMessage :: Annex (Maybe String)
getCommitMessage = 
	outputOfAnnexHook commitMessageAnnexHook annexCommitMessageCommand
		<|>
	(annexCommitMessage <$> Annex.getGitConfig)

{- Stages the journal, and commits staged changes to the branch. -}
commit :: String -> Annex ()
commit = whenM (journalDirty gitAnnexJournalDir) . forceCommit

{- Commits the current index to the branch even without any journalled
 - changes. -}
forceCommit :: String -> Annex ()
forceCommit message = lockJournal $ \jl ->
	stageJournal jl $ do
		ref <- getBranch
		commitIndex jl ref message [fullname]

{- Commits the staged changes in the index to the branch.
 - 
 - Ensures that the branch's index file is first updated to merge the state
 - of the branch at branchref, before running the commit action. This
 - is needed because the branch may have had changes pushed to it, that
 - are not yet reflected in the index.
 - 
 - The branchref value can have been obtained using getBranch at any
 - previous point, though getting it a long time ago makes the race
 - more likely to occur.
 -
 - Note that changes may be pushed to the branch at any point in time!
 - So, there's a race. If the commit is made using the newly pushed tip of
 - the branch as its parent, and that ref has not yet been merged into the
 - index, then the result is that the commit will revert the pushed
 - changes, since they have not been merged into the index. This race
 - is detected and another commit made to fix it.
 -
 - (It's also possible for the branch to be overwritten,
 - losing the commit made here. But that's ok; the data is still in the
 - index and will get committed again later.)
 -}
commitIndex :: JournalLocked -> Git.Ref -> String -> [Git.Ref] -> Annex ()
commitIndex jl branchref message parents = do
	showStoringStateAction
	commitIndex' jl branchref message message 0 parents
commitIndex' :: JournalLocked -> Git.Ref -> String -> String -> Integer -> [Git.Ref] -> Annex ()
commitIndex' jl branchref message basemessage retrynum parents = do
	updateIndex jl branchref
	cmode <- annexCommitMode <$> Annex.getGitConfig
	committedref <- inRepo $ Git.Branch.commitAlways cmode message fullname parents
	setIndexSha committedref
	parentrefs <- commitparents <$> catObject committedref
	when (racedetected branchref parentrefs) $
		fixrace committedref parentrefs
  where
	-- look for "parent ref" lines and return the refs
	commitparents = map (Git.Ref . snd) . filter isparent .
		map (toassoc . L.toStrict) . L.split newline
	newline = fromIntegral (ord '\n')
	toassoc = separate' (== (fromIntegral (ord ' ')))
	isparent (k,_) = k == "parent"
		
	{- The race can be detected by checking the commit's
	 - parent, which will be the newly pushed branch,
	 - instead of the expected ref that the index was updated to. -}
	racedetected expectedref parentrefs
		| expectedref `elem` parentrefs = False -- good parent
		| otherwise = True -- race!
		
	{- To recover from the race, union merge the lost refs
	 - into the index. -}
	fixrace committedref lostrefs = do
		showSideAction "recovering from race"
		let retrynum' = retrynum+1
		-- small sleep to let any activity that caused
		-- the race settle down
		liftIO $ threadDelay (100000 + fromInteger retrynum')
		mergeIndex jl lostrefs
		let racemessage = basemessage ++ " (recovery from race #" ++ show retrynum' ++ "; expected commit parent " ++ show branchref ++ " but found " ++ show lostrefs ++ " )"
		commitIndex' jl committedref racemessage basemessage retrynum' [committedref]

{- Lists all files on the branch. including ones in the journal
 - that have not been committed yet. 
 -
 - There may be duplicates in the list, when the journal has files that
 - have not been written to the branch yet.
 - 
 - In a read-only repository that has other git-annex branches that have
 - not been merged in, returns Nothing, because it's not possible to
 - efficiently handle that.
 -}
files :: Annex (Maybe ([OsPath], IO Bool))
files = do
	st <- update
        if not (null (unmergedRefs st))
		then return Nothing
		else do
			(bfs, cleanup) <- branchFiles
			jfs <- journalledFiles
			pjfs <- journalledFilesPrivate
			-- ++ forces the content of the first list to be
			-- buffered in memory, so use journalledFiles,
			-- which should be much smaller most of the time.
			-- branchFiles will stream as the list is consumed.
			let l = jfs ++ pjfs ++ bfs
			return (Just (l, cleanup))

{- Lists all files currently in the journal, but not files in the private
 - journal. -}
journalledFiles :: Annex [OsPath]
journalledFiles = getJournalledFilesStale gitAnnexJournalDir

journalledFilesPrivate :: Annex [OsPath]
journalledFilesPrivate = ifM privateUUIDsKnown
	( getJournalledFilesStale gitAnnexPrivateJournalDir
	, return []
	)

{- Files in the branch, not including any from journalled changes,
 - and without updating the branch. -}
branchFiles :: Annex ([OsPath], IO Bool)
branchFiles = withIndex $ inRepo branchFiles'

branchFiles' :: Git.Repo -> IO ([OsPath], IO Bool)
branchFiles' = Git.Command.pipeNullSplit'' toOsPath $
	lsTreeParams Git.LsTree.LsTreeRecursive (Git.LsTree.LsTreeLong False)
		fullname
		[Param "--name-only"]

{- Populates the branch's index file with the current branch contents.
 - 
 - This is only done when the index doesn't yet exist, and the index 
 - is used to build up changes to be committed to the branch, and merge
 - in changes from other branches.
 -}
genIndex :: Git.Repo -> IO ()
genIndex g = Git.UpdateIndex.streamUpdateIndex g
	[Git.UpdateIndex.lsTree fullname g]

{- Merges the specified refs into the index.
 - Any changes staged in the index will be preserved. -}
mergeIndex :: JournalLocked -> [Git.Ref] -> Annex ()
mergeIndex jl branches = do
	prepareModifyIndex jl
	withHashObjectHandle $ \hashhandle ->
		withCatFileHandle $ \ch ->
			inRepo $ \g -> Git.UnionMerge.mergeIndex hashhandle ch g branches

{- Removes any stale git lock file, to avoid git falling over when
 - updating the index.
 -
 - Since all modifications of the index are performed inside this module,
 - and only when the journal is locked, the fact that the journal has to be
 - locked when this is called ensures that no other process is currently
 - modifying the index. So any index.lock file must be stale, caused
 - by git running when the system crashed, or the repository's disk was
 - removed, etc.
 -}
prepareModifyIndex :: JournalLocked -> Annex ()
prepareModifyIndex _jl = do
	index <- fromRepo gitAnnexIndex
	void $ liftIO $ tryIO $
		removeFile (index <> literalOsPath ".lock")

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex = withIndex' False
withIndex' :: Bool -> Annex a -> Annex a
withIndex' bootstrapping a = withIndexFile AnnexIndexFile $ \f -> do
	checkIndexOnce $ unlessM (liftIO $ doesFileExist f) $ do
		unless bootstrapping create
		createAnnexDirectory $ takeDirectory f
		unless bootstrapping $ inRepo genIndex
	a

{- Updates the branch's index to reflect the current contents of the branch.
 - Any changes staged in the index will be preserved.
 -
 - Compares the ref stored in the lock file with the current
 - ref of the branch to see if an update is needed.
 -}
updateIndex :: JournalLocked -> Git.Ref -> Annex ()
updateIndex jl branchref = whenM (needUpdateIndex branchref) $
	forceUpdateIndex jl branchref

forceUpdateIndex :: JournalLocked -> Git.Ref -> Annex ()
forceUpdateIndex jl branchref = do
	withIndex $ mergeIndex jl [fullname]
	setIndexSha branchref

{- Checks if the index needs to be updated. -}
needUpdateIndex :: Git.Ref -> Annex Bool
needUpdateIndex branchref = do
	f <- fromRepo gitAnnexIndexStatus
	committedref <- Git.Ref . firstLine' <$>
		liftIO (catchDefaultIO mempty $ F.readFile' f)
	return (committedref /= branchref)

{- Record that the branch's index has been updated to correspond to a
 - given sha of the branch. -}
setIndexSha :: Git.Sha -> Annex ()
setIndexSha ref = do
	f <- fromRepo gitAnnexIndexStatus
	writeLogFile f $ fromRef ref ++ "\n"
	runAnnexHook postUpdateAnnexHook annexPostUpdateCommand

{- Stages the journal into the index, and runs an action that
 - commits the index to the branch. Note that the action is run
 - inside withIndex so will automatically use the branch's index.
 -
 - Before staging, this removes any existing git index file lock.
 - This is safe to do because stageJournal is the only thing that
 - modifies this index file, and only one can run at a time, because
 - the journal is locked. So any existing git index file lock must be
 - stale, and the journal must contain any data that was in the process
 - of being written to the index file when it crashed.
 -}
stageJournal :: JournalLocked -> Annex () -> Annex ()
stageJournal jl commitindex = withIndex $ withOtherTmp $ \tmpdir -> do
	prepareModifyIndex jl
	g <- gitRepo
	st <- getState
	let dir = gitAnnexJournalDir st g
	(jlogf, jlogh) <- openjlog tmpdir
	withHashObjectHandle $ \h ->
		withJournalHandle gitAnnexJournalDir $ \jh ->
			Git.UpdateIndex.streamUpdateIndex g
				[genstream dir h jh jlogh]
	commitindex
	liftIO $ cleanup dir jlogh jlogf
  where
	genstream dir h jh jlogh streamer = readDirectory jh >>= \case
		Nothing -> return ()
		Just file -> do
			let file' = toOsPath file
			let path = dir </> file'
			unless (file' `elem` dirCruft) $ whenM (isfile path) $ do
				sha <- Git.HashObject.hashFile h path
				B.hPutStr jlogh (file <> "\n")
				streamer $ Git.UpdateIndex.updateIndexLine
					sha TreeFile (asTopFilePath $ fileJournal file')
			genstream dir h jh jlogh streamer
	isfile file = isRegularFile <$> R.getFileStatus (fromOsPath file)
	-- Clean up the staged files, as listed in the temp log file.
	-- The temp file is used to avoid needing to buffer all the
	-- filenames in memory.
	cleanup dir jlogh jlogf = do
		hFlush jlogh
		hSeek jlogh AbsoluteSeek 0
		stagedfs <- lines <$> hGetContents jlogh
		mapM_ (removeFile . (dir </>) . toOsPath) stagedfs
		hClose jlogh
		removeWhenExistsWith removeFile jlogf
	openjlog tmpdir = liftIO $ openTmpFileIn tmpdir (literalOsPath "jlog")

getLocalTransitions :: Annex Transitions
getLocalTransitions = 
	parseTransitionsStrictly "local"
		<$> getLocal transitionsLog

{- This is run after the refs have been merged into the index,
 - but before the result is committed to the branch.
 - (Which is why it's passed the contents of the local branches's
 - transition log before that merge took place.)
 -
 - When the refs contain transitions that have not yet been done locally,
 - the transitions are performed on the index, and a new branch
 - is created from the result.
 -
 - When there are transitions recorded locally that have not been done
 - to the remote refs, the transitions are performed in the index,
 - and committed to the existing branch. In this case, the untransitioned
 - remote refs cannot be merged into the branch (since transitions
 - throw away history), so they are added to the list of refs to ignore,
 - to avoid re-merging content from them again.
 -}
handleTransitions :: JournalLocked -> Transitions -> [Git.Ref] -> Annex Bool
handleTransitions jl localts refs = do
	remotets <- mapM getRefTransitions refs
	if all (localts ==) remotets
		then return False
		else do
			let m = M.fromList (zip refs remotets)
			let allts = combineTransitions (localts:remotets)
			let (transitionedrefs, untransitionedrefs) =
				partition (\r -> M.lookup r m == Just allts) refs
			performTransitionsLocked jl allts (localts /= allts) transitionedrefs
			ignoreRefs untransitionedrefs
			return True

{- Performs the specified transitions on the contents of the index file,
 - commits it to the branch, or creates a new branch.
 -}
performTransitions :: Transitions -> Bool -> [Ref] -> Annex ()
performTransitions ts neednewlocalbranch transitionedrefs = lockJournal $ \jl ->
	performTransitionsLocked jl ts neednewlocalbranch transitionedrefs
performTransitionsLocked :: JournalLocked -> Transitions -> Bool -> [Ref] -> Annex ()
performTransitionsLocked jl ts neednewlocalbranch transitionedrefs = do
	-- For simplicity & speed, we're going to use the Annex.Queue to
	-- update the git-annex branch, while it usually holds changes
	-- for the head branch. Flush any such changes.
	Annex.Queue.flush
	-- Stop any running git cat-files, to ensure that the
	-- getStaged calls below use the current index, and not some older
	-- one.
	catFileStop
	withIndex $ do
		prepareModifyIndex jl
		run $ mapMaybe getTransitionCalculator tlist
		Annex.Queue.flush
		if neednewlocalbranch
			then do
				cmode <- annexCommitMode <$> Annex.getGitConfig
				-- Creating a new empty branch must happen
				-- atomically, so if this is interrupted,
				-- it will not leave the new branch created
				-- but without exports grafted in.
				c <- inRepo $ Git.Branch.commitShaAlways
					cmode message transitionedrefs
				void $ regraftexports c
			else do
				ref <- getBranch
				ref' <- regraftexports ref
				commitIndex jl ref' message
					(nub $ fullname:transitionedrefs)
  where
	message
		| neednewlocalbranch && null transitionedrefs = "new branch for transition " ++ tdesc
		| otherwise = "continuing transition " ++ tdesc
	tdesc = show $ map describeTransition tlist
	tlist = knownTransitionList ts

	{- The changes to make to the branch are calculated and applied to
	 - the branch directly, rather than going through the journal,
	 - which would be innefficient. (And the journal is not designed
	 - to hold changes to every file in the branch at once.)
	 -
	 - When a file in the branch is changed by transition code,
	 - its new content is remembered and fed into the code for subsequent
	 - transitions.
	 -}
	run [] = noop
	run changers = do
		config <- Annex.getGitConfig
		trustmap <- calcTrustMap <$> getStaged trustLog
		remoteconfigmap <- calcRemoteConfigMap <$> getStaged remoteLog
		-- partially apply, improves performance
		let changers' = map (\c -> c trustmap remoteconfigmap config) changers
		(fs, cleanup) <- branchFiles
		forM_ fs $ \f -> do
			content <- getStaged f
			apply changers' f content
		liftIO $ void cleanup
	
	apply [] _ _ = return ()
	apply (changer:rest) file content = case changer file content of
		PreserveFile -> apply rest file content
		ChangeFile builder -> do
			let content' = toLazyByteString builder
			if L.null content'
				then do
					Annex.Queue.addUpdateIndex
						=<< inRepo (Git.UpdateIndex.unstageFile file)
					-- File is deleted; can't run any other
					-- transitions on it.
					return ()
				else do
					sha <- hashBlob content'
					Annex.Queue.addUpdateIndex $ Git.UpdateIndex.pureStreamer $
						Git.UpdateIndex.updateIndexLine sha TreeFile (asTopFilePath file)
					apply rest file content'

	-- Trees mentioned in export.log were grafted into the old
	-- git-annex branch to make sure they remain available.
	-- Re-graft the trees.
	regraftexports parent = do
		l <- exportedTreeishes . M.elems . parseExportLogMap
			<$> getStaged exportLog
		c <- regraft l parent
		inRepo $ Git.Branch.update' fullname c
		setIndexSha c
		return c
	  where
		regraft [] c = pure c
		regraft (et:ets) c =
			-- Verify that the tree object exists.
			catObjectDetails et >>= \case
				Just _ ->
					prepRememberTreeish et graftpoint c
						>>= regraft ets
				Nothing -> regraft ets c
		graftpoint = asTopFilePath exportTreeGraftPoint

checkBranchDifferences :: Git.Ref -> Annex ()
checkBranchDifferences ref = do
	theirdiffs <- allDifferences . parseDifferencesLog
		<$> catFile ref differenceLog
	mydiffs <- annexDifferences <$> Annex.getGitConfig
	when (theirdiffs /= mydiffs) $
		giveup "Remote repository is tuned in incompatible way; cannot be merged with local repository."

ignoreRefs :: [Git.Sha] -> Annex ()
ignoreRefs rs = do
	old <- getIgnoredRefs
	let s = S.unions [old, S.fromList rs]
	f <- fromRepo gitAnnexIgnoredRefs
	writeLogFile f $
		unlines $ map fromRef $ S.elems s

getIgnoredRefs :: Annex (S.Set Git.Sha)
getIgnoredRefs = 
	S.fromList . mapMaybe Git.Sha.extractSha . fileLines' <$> content
  where
	content = do
		f <- fromRepo gitAnnexIgnoredRefs
		liftIO $ catchDefaultIO mempty $ F.readFile' f

addMergedRefs :: [(Git.Sha, Git.Branch)] -> Annex ()
addMergedRefs [] = return ()
addMergedRefs new = do
	old <- getMergedRefs'
	-- Keep only the newest sha for each branch.
	let l = nubBy ((==) `on` snd) (new ++ old)
	f <- fromRepo gitAnnexMergedRefs
	writeLogFile f $
		unlines $ map (\(s, b) -> fromRef s ++ '\t' : fromRef b) l

getMergedRefs :: Annex (S.Set Git.Sha)
getMergedRefs = S.fromList . map fst <$> getMergedRefs'

getMergedRefs' :: Annex [(Git.Sha, Git.Branch)]
getMergedRefs' = do
	f <- fromRepo gitAnnexMergedRefs
	s <- liftIO $ catchDefaultIO mempty $ F.readFile' f
	return $ map parse $ fileLines' s
  where
	parse l = 
		let (s, b) = separate' (== (fromIntegral (ord '\t'))) l
		in (Ref s, Ref b)

{- Grafts a treeish into the branch at the specified location,
 - and then removes it. This ensures that the treeish won't get garbage
 - collected, and will always be available as long as the git-annex branch
 - is available.
 -
 - Returns the sha of the git commit made to the git-annex branch.
 -}
rememberTreeish :: Git.Ref -> TopFilePath -> Annex Git.Sha
rememberTreeish treeish graftpoint = lockJournal $ \jl -> do
	branchref <- getBranch
	updateIndex jl branchref
	c <- prepRememberTreeish treeish graftpoint branchref
	inRepo $ Git.Branch.update' fullname c
	-- The tree in c is the same as the tree in branchref,
	-- and the index was updated to that above, so it's safe to
	-- say that the index contains c.
	setIndexSha c
	return c

{- Create a series of commits that graft a tree onto the parent commit,
 - and then remove it. -}
prepRememberTreeish :: Git.Ref -> TopFilePath -> Git.Ref -> Annex Git.Sha
prepRememberTreeish treeish graftpoint parent = do
	origtree <- fromMaybe (giveup "unable to determine git-annex branch tree") <$>
		inRepo (Git.Ref.tree parent)
	addedt <- inRepo $ Git.Tree.graftTree treeish graftpoint origtree
	cmode <- annexCommitMode <$> Annex.getGitConfig
	c <- inRepo $ Git.Branch.commitTree cmode
		["graft"] [parent] addedt
	inRepo $ Git.Branch.commitTree cmode
		["graft cleanup"] [c] origtree

{- UnmergedBranches is used to indicate when a value was calculated in a
 - read-only repository that has other git-annex branches that have not
 - been merged in. The value does not include information from those
 - branches.
 -}
data UnmergedBranches t
	= UnmergedBranches t 
	| NoUnmergedBranches t

type FileContents t b = Maybe (t, OsPath, Maybe (L.ByteString, Maybe b))

{- Runs an action on the content of selected files from the branch.
 - This is much faster than reading the content of each file in turn,
 - because it lets git cat-file stream content without blocking.
 -
 - The action is passed a callback that it can repeatedly call to read
 - the next file and its contents. When there are no more files, the
 - callback will return Nothing.
 -
 - Returns the accumulated result of the callback, as well as the sha of
 - the branch at the point it was read.
 -}
overBranchFileContents
	:: Bool
	-- ^ Should files in the journal be ignored? When False,
	-- the content of journalled files is combined with files in the
	-- git-annex branch. And also, at the end, the callback is run
	-- on each journalled file, in case some journalled files are new
	-- files that do not yet appear in the branch. Note that this means
	-- the callback can be run more than once on the same filename,
	-- and in this case it's also possible for the callback to be
	-- passed some of the same file content repeatedly.
	-> (OsPath -> Maybe v)
	-> (Annex (FileContents v Bool) -> Annex a)
	-> Annex (UnmergedBranches (a, Git.Sha))
overBranchFileContents ignorejournal select go = do
	st <- update
	let st' = if ignorejournal
		then st { journalIgnorable = True }
		else st
	v <- overBranchFileContents' select go st'
	return $ if not (null (unmergedRefs st))
		then UnmergedBranches v
		else NoUnmergedBranches v

overBranchFileContents'
	:: (OsPath -> Maybe v)
	-> (Annex (FileContents v Bool) -> Annex a)
	-> BranchState
	-> Annex (a, Git.Sha)
overBranchFileContents' select go st = do
	g <- Annex.gitRepo
	branchsha <- getBranch
	(l, cleanup) <- inRepo $ Git.LsTree.lsTree
		Git.LsTree.LsTreeRecursive
		(Git.LsTree.LsTreeLong False)
		branchsha
	let select' f = fmap (\v -> (v, f)) (select f)
	buf <- liftIO newEmptyMVar
	let go' reader = go $ liftIO reader >>= \case
		Just ((v, f), content) -> do
			content' <- checkjournal f content >>= return . \case
				Nothing -> Nothing
				Just c -> Just (c, Just False)
			return (Just (v, f, content'))
		Nothing
			| journalIgnorable st -> return Nothing
			| otherwise ->
				overJournalFileContents' buf (handlestale branchsha) select
	res <- catObjectStreamLsTree l (select' . getTopFilePath . Git.LsTree.file) g go'
		`finally` liftIO (void cleanup)
	return (res, branchsha)
  where
	checkjournal f branchcontent
		| journalIgnorable st = return branchcontent
		| otherwise = getJournalFileStale (GetPrivate True) f >>= return . \case
			NoJournalledContent -> branchcontent
			JournalledContent journalledcontent ->
				Just journalledcontent
			PossiblyStaleJournalledContent journalledcontent ->
				Just (fromMaybe mempty branchcontent <> journalledcontent)
	
	handlestale branchsha f journalledcontent = do
		-- This is expensive, but happens only when there is a
		-- private journal file.
		branchcontent <- getRef branchsha f
		return (combineStaleJournalWithBranch branchcontent journalledcontent, Just True)

combineStaleJournalWithBranch :: L.ByteString -> L.ByteString -> L.ByteString
combineStaleJournalWithBranch branchcontent journalledcontent =
	branchcontent <> journalledcontent

{- Like overBranchFileContents but only reads the content of journalled
 - files.
 -}
overJournalFileContents
	:: (OsPath -> L.ByteString -> Annex (L.ByteString, Maybe b))
	-- ^ Called with the journalled file content when the journalled
	-- content may be stale or lack information committed to the
	-- git-annex branch.
	-> (OsPath -> Maybe v)
	-> (Annex (FileContents v b) -> Annex a)
	-> Annex a
overJournalFileContents handlestale select go = do
	buf <- liftIO newEmptyMVar
	go $ overJournalFileContents' buf handlestale select

overJournalFileContents'
	:: MVar ([OsPath], [OsPath])
	-> (OsPath -> L.ByteString -> Annex (L.ByteString, Maybe b))
	-> (OsPath -> Maybe a)
	-> Annex (FileContents a b)
overJournalFileContents' buf handlestale select =
	liftIO (tryTakeMVar buf) >>= \case
		Nothing -> do
			jfs <- journalledFiles
			pjfs <- journalledFilesPrivate
			drain jfs pjfs
		Just (jfs, pjfs) -> drain jfs pjfs
  where
	drain fs pfs = case getnext fs pfs of
		Just (v, f, fs', pfs') -> do
			liftIO $ putMVar buf (fs', pfs')
			content <- getJournalFileStale (GetPrivate True) f >>= \case
				NoJournalledContent -> return Nothing
				JournalledContent journalledcontent ->
					return (Just (journalledcontent, Nothing))
				PossiblyStaleJournalledContent journalledcontent ->
					Just <$> handlestale f journalledcontent
			return (Just (v, f, content))
		Nothing -> do
			liftIO $ putMVar buf ([], [])
			return Nothing
	
	getnext [] [] = Nothing
	getnext (f:fs) pfs = case select f of
		Nothing -> getnext fs pfs
		Just v -> Just (v, f, fs, pfs)
	getnext [] (pf:pfs) = case select pf of
		Nothing -> getnext [] pfs
		Just v -> Just (v, pf, [], pfs)

{- Check if the git-annex branch has been updated from the oldtree.
 - If so, returns the tuple of the old and new trees. -}
updatedFromTree :: Git.Sha -> Annex (Maybe (Git.Sha, Git.Sha))
updatedFromTree oldtree =
	inRepo (Git.Ref.tree fullname) >>= \case
		Just currtree | currtree /= oldtree ->
			return $ Just (oldtree, currtree)
		_ -> return Nothing
