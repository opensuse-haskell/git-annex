{- git-annex command
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Command.Unused where

import Command
import Logs.Unused
import Annex.Content
import Logs.Location
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.RefLog
import qualified Git.LsFiles as LsFiles
import qualified Git.DiffTree as DiffTree
import qualified Remote
import qualified Annex.Branch
import Annex.CatFile
import Annex.WorkTree
import Types.RefSpec
import Git.Types
import Git.Sha
import Git.FilePath
import Config
import Logs.View (is_branchView)
import Annex.BloomFilter
import qualified Database.Keys
import Annex.InodeSentinal
import Backend.GitRemoteAnnex (isGitRemoteAnnexKey)

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Data.Char

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "unused" SectionMaintenance "look for unused file content"
		paramNothing (seek <$$> optParser)

data UnusedOptions = UnusedOptions
	{ fromRemote :: Maybe RemoteName
	, refSpecOption :: Maybe RefSpec
	}

optParser :: CmdParamsDesc -> Parser UnusedOptions
optParser _ = UnusedOptions
	<$> optional (strOption
		( long "from" <> short 'f' <> metavar paramRemote
		<> completeRemotes
		<> help "remote to check for unused content"
		))
	<*> optional (option (eitherReader parseRefSpec)
		( long "used-refspec" <> metavar paramRefSpec
		<> help "refs to consider used (default: all branches)"
		))

seek :: UnusedOptions -> CommandSeek
seek = commandAction . start

start :: UnusedOptions -> CommandStart
start o = do
	cfgrefspec <- fromMaybe allRefSpec . annexUsedRefSpec
		<$> Annex.getGitConfig
	let refspec = fromMaybe cfgrefspec (refSpecOption o)
	let (name, perform) = case fromRemote o of
		Nothing -> (".", checkUnused refspec)
		Just "." -> (".", checkUnused refspec)
		Just "here" -> (".", checkUnused refspec)
		Just n -> (n, checkRemoteUnused n refspec)
	starting "unused" (ActionItemOther (Just (UnquotedString name))) (SeekInput []) perform

checkUnused :: RefSpec -> CommandPerform
checkUnused refspec = chain 0
	[ check "" unusedMsg $ findunused =<< Annex.getRead Annex.fast
	, check "bad" staleBadMsg $ staleKeysPrune gitAnnexBadDir False
	, check "tmp" staleTmpMsg $ staleKeysPrune gitAnnexTmpObjectDir True
	]
  where
	findunused True = do
		showNote "fast mode enabled; only finding stale files"
		return []
	findunused False = do
		showAction "checking for unused data"
		excludeReferenced refspec =<< listKeys InAnnex
	chain _ [] = next $ return True
	chain v (a:as) = do
		v' <- a v
		chain v' as

checkRemoteUnused :: RemoteName -> RefSpec -> CommandPerform
checkRemoteUnused remotename refspec = go =<< Remote.nameToUUID remotename
  where
	go u = do
		showAction "checking for unused data"
		r <- Remote.byUUID u
		_ <- check "" (remoteUnusedMsg r remotename) (remoteunused u) 0
		next $ return True
	remoteunused u = loggedKeysFor u >>= \case
		Just ks -> filter (not . isGitRemoteAnnexKey u)
			<$> excludeReferenced refspec ks
		Nothing -> giveup "This repository is read-only."

check :: String -> ([(Int, Key)] -> String) -> Annex [Key] -> Int -> Annex Int
check fileprefix msg a c = do
	l <- a
	let unusedlist = number c l
	unless (null l) $
		showLongNote $ UnquotedString $ msg unusedlist
	maybeAddJSONField
		((if null fileprefix then "unused" else fileprefix) ++ "-list")
		(M.fromList $ map (\(n,  k) -> (T.pack (show n), serializeKey k)) unusedlist)
	updateUnusedLog (toOsPath fileprefix) (M.fromList unusedlist)
	return $ c + length l

number :: Int -> [a] -> [(Int, a)]
number _ [] = []
number n (x:xs) = (n+1, x) : number (n+1) xs

table :: [(Int, Key)] -> [String]
table l = "  NUMBER  KEY" : map cols l
  where
	cols (n,k) = "  " ++ pad 6 (show n) ++ "  " ++ serializeKey k
	pad n s = s ++ replicate (n - length s) ' '

staleTmpMsg :: [(Int, Key)] -> String
staleTmpMsg t = unlines $ 
	["Some partially transferred data exists in temporary files:"]
	++ table t ++ [dropMsg Nothing]

staleBadMsg :: [(Int, Key)] -> String
staleBadMsg t = unlines $ 
	["Some corrupted files have been preserved by fsck, just in case:"]
	++ table t ++ [dropMsg Nothing]

unusedMsg :: [(Int, Key)] -> String
unusedMsg u = unusedMsg' u
	["Some annexed data is no longer used by any files:"]
	[dropMsg Nothing]
unusedMsg' :: [(Int, Key)] -> [String] -> [String] -> String
unusedMsg' u mheader mtrailer = unlines $
	mheader ++
	table u ++
	["(To see where this data was previously used, run: git annex whereused --historical --unused"] ++
	mtrailer

remoteUnusedMsg :: Maybe Remote -> RemoteName -> [(Int, Key)] -> String
remoteUnusedMsg mr remotename u = unusedMsg' u
	["Some annexed data on " ++ remotename ++ " is not used by any files:"]
	(if isJust mr then [dropMsg (Just remotename)] else [])

dropMsg :: Maybe RemoteName -> String
dropMsg Nothing = dropMsg' ""
dropMsg (Just remotename) = dropMsg' $ " --from " ++ remotename
dropMsg' :: String -> String
dropMsg' s = "\nTo remove unwanted data: git-annex dropunused" ++ s ++ " NUMBER\n"

{- Finds keys in the list that are not referenced in the git repository.
 -
 - Strategy:
 -
 - Pass keys through these filters in order, only creating each bloom
 - filter on demand if the previous one didn't filter out all keys.
 -
 - 1. Bloom filter containing all keys referenced by files in the work tree.
 -    This is the fastest one to build and will filter out most keys.
 - 2. Bloom filter containing all keys in the diff from the work tree to
 -    the index.
 - 3. Associated files filter. An unlocked file may have had its content
 -    added to the annex (by eg, git diff running the smudge filter),
 -    but the new key is not yet staged in the index. But if so, it will 
 -    have an associated file.
 - 4. Bloom filter containing all keys in the diffs between the index and
 -    branches matching the RefSpec. (This can take quite a while to build).
 -}
excludeReferenced :: RefSpec -> [Key] -> Annex [Key]
excludeReferenced refspec ks = runbloomfilter withKeysReferencedM ks
	>>= runbloomfilter withKeysReferencedDiffIndex
	>>= runfilter associatedFilesFilter
	>>= runbloomfilter (withKeysReferencedDiffGitRefs refspec)
  where
	runfilter _ [] = return [] -- optimisation
	runfilter a l = a l
	runbloomfilter a = runfilter $ \l -> bloomFilter l <$> genBloomFilter a

{- Given an initial value, accumulates the value over each key
 - referenced by files in the working tree. -}
withKeysReferenced :: v -> (Key -> OsPath -> v -> Annex v) -> Annex v
withKeysReferenced initial = withKeysReferenced' Nothing initial

{- Runs an action on each referenced key in the working tree. -}
withKeysReferencedM :: (Key -> Annex ()) -> Annex ()
withKeysReferencedM a = withKeysReferenced' Nothing () calla
  where
	calla k _ _ = a k

{- Folds an action over keys and files referenced in a particular directory. -}
withKeysFilesReferencedIn :: OsPath -> v -> (Key -> OsPath -> v -> Annex v) -> Annex v
withKeysFilesReferencedIn = withKeysReferenced' . Just

withKeysReferenced' :: Maybe OsPath -> v -> (Key -> OsPath -> v -> Annex v) -> Annex v
withKeysReferenced' mdir initial a = do
	(files, clean) <- getfiles
	r <- go initial files
	liftIO $ void clean
	return r
  where
	getfiles = case mdir of
		Nothing -> ifM isBareRepo
			( return ([], return True)
			, do
				top <- fromRepo Git.repoPath
				inRepo $ LsFiles.allFiles [] [top]
			)
		Just dir -> inRepo $ LsFiles.inRepo [] [dir]
	go v [] = return v
	go v (f:fs) = do
		mk <- lookupKey f
		case mk of
			Nothing -> go v fs
			Just k -> do
				!v' <- a k f v
				go v' fs

withKeysReferencedDiffGitRefs :: RefSpec -> (Key -> Annex ()) -> Annex ()
withKeysReferencedDiffGitRefs refspec a = do
	rs <- relevantrefs <$> inRepo (Git.Command.pipeReadStrict [Param "show-ref"])
	shaHead <- maybe (return Nothing) (inRepo . Git.Ref.sha)
		=<< inRepo Git.Branch.currentUnsafe
	let haveHead = any (\(shaRef, _) -> Just shaRef == shaHead) rs
	let rs' = map snd (nubRefs rs)
	usedrefs <- applyRefSpec refspec rs' (getreflog rs')
	forM_ (if haveHead then usedrefs else Git.Ref.headRef : usedrefs) $
		withKeysReferencedDiffGitRef a
  where
	relevantrefs = map (\(r, h) -> (Git.Ref r, Git.Ref h)) .
		filter ourbranches .
		map (separate' (== (fromIntegral (ord ' ')))) .
		S8.lines
	nubRefs = nubBy (\(x, _) (y, _) -> x == y)
	ourbranchend = S.cons (fromIntegral (ord '/')) (Git.fromRef' Annex.Branch.name)
	ourbranches (_, b) = not (ourbranchend `S.isSuffixOf` b)
		&& not ("refs/synced/" `S.isPrefixOf` b)
		&& not ("refs/annex/" `S.isPrefixOf` b)
		&& not (is_branchView (Git.Ref b))
	getreflog rs = inRepo $ Git.RefLog.getMulti rs

{- Runs an action on keys referenced in the given Git reference which
 - differ from those referenced in the index. -}
withKeysReferencedDiffGitRef :: (Key -> Annex ()) -> Git.Ref -> Annex ()
withKeysReferencedDiffGitRef a ref = do
	showAction $ UnquotedString $ "checking " ++ Git.Ref.describe ref
	withKeysReferencedDiff a
		(inRepo $ DiffTree.diffIndex ref)
		DiffTree.srcsha

{- Runs an action on keys referenced in the index which differ from the
 - work tree. -}
withKeysReferencedDiffIndex :: (Key -> Annex ()) -> Annex ()
withKeysReferencedDiffIndex a = unlessM (isBareRepo) $
	withKeysReferencedDiff a
		(inRepo $ DiffTree.diffFiles [])
		DiffTree.srcsha

withKeysReferencedDiff :: (Key -> Annex ()) -> (Annex ([DiffTree.DiffTreeItem], IO Bool)) -> (DiffTree.DiffTreeItem -> Sha) -> Annex ()
withKeysReferencedDiff a getdiff extractsha = do
	(ds, clean) <- getdiff
	forM_ ds go
	liftIO $ void clean
  where
  	go d = do
		let sha = extractsha d
		unless (sha `elem` nullShas) $
			catKey sha >>= maybe noop a

{- Filters out keys that have an associated file that's not modified. -}
associatedFilesFilter :: [Key] -> Annex [Key]
associatedFilesFilter = filterM go
  where
	go k = do
		cs <- Database.Keys.getInodeCaches k
		if null cs
			then return True
			else checkunmodified cs
				=<< Database.Keys.getAssociatedFiles k
	checkunmodified _ [] = return True
	checkunmodified cs (f:fs) = do
		relf <- fromRepo $ fromTopFilePath f
		ifM (sameInodeCache relf cs)
			( return False
			, checkunmodified cs fs
			)

data UnusedMaps = UnusedMaps
	{ unusedMap :: UnusedMap
	, unusedBadMap :: UnusedMap
	, unusedTmpMap :: UnusedMap
	}

withUnusedMaps :: (UnusedMaps -> Int -> CommandStart) -> CmdParams -> CommandSeek
withUnusedMaps a params = do
	unused <- readUnusedMap (literalOsPath "")
	unusedbad <- readUnusedMap (literalOsPath "bad")
	unusedtmp <- readUnusedMap (literalOsPath "tmp")
	let m = unused `M.union` unusedbad `M.union` unusedtmp
	let unusedmaps = UnusedMaps unused unusedbad unusedtmp
	commandActions $ map (a unusedmaps) $ concatMap (unusedSpec m) params

unusedSpec :: UnusedMap -> String -> [Int]
unusedSpec m spec
	| spec == "all" = if M.null m
		then []
		else [fst (M.findMin m)..fst (M.findMax m)]
	| "-" `isInfixOf` spec = range $ separate (== '-') spec
	| otherwise = maybe badspec (: []) (readish spec)
  where
	range (a, b) = case (readish a, readish b) of
		(Just x, Just y) -> [x..y]
		_ -> badspec
	badspec = giveup $ "Expected number or range, not \"" ++ spec ++ "\""

{- Seek action for unused content. Finds the number in the maps, and
 - calls one of 3 actions, depending on the type of unused file. -}
startUnused
	:: (Int -> Key -> CommandStart)
	-> (Int -> Key -> CommandStart) 
	-> (Int -> Key -> CommandStart)
	-> UnusedMaps -> Int -> CommandStart
startUnused unused badunused tmpunused maps n = search
	[ (unusedMap maps, unused)
	, (unusedBadMap maps, badunused)
	, (unusedTmpMap maps, tmpunused)
	]
  where
	search [] = giveup $ show n ++ " not valid (run git annex unused for list)"
	search ((m, a):rest) =
		case M.lookup n m of
			Nothing -> search rest
			Just key -> a n key
