{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 - Copyright 2013 Antoine Beaupré
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.List where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.Ord
import qualified Data.ByteString.Char8 as B8

import Command
import Remote
import qualified Annex
import Logs.Trust
import Logs.UUID
import Annex.UUID
import Git.Types (RemoteName)
import Utility.Tuple
import Utility.SafeOutput

cmd :: Command
cmd = noCommit $ withAnnexOptions [annexedMatchingOptions] $
	command "list" SectionQuery 
		"show which remotes contain files"
		paramPaths (seek <$$> optParser)

data ListOptions = ListOptions
	{ listThese :: CmdParams
	, allRepos :: Bool
	}

optParser :: CmdParamsDesc -> Parser ListOptions
optParser desc = ListOptions
	<$> cmdParams desc
	<*> switch
		( long "allrepos"
		<> help "show all repositories, not only remotes"
		)

seek :: ListOptions -> CommandSeek
seek o = do
	list <- getList o
	printHeader list
	let seeker = AnnexedFileSeeker
		{ startAction = const $ start list
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}
	withFilesInGitAnnex ww seeker =<< workTreeItems ww (listThese o)
  where
	ww = WarnUnmatchLsFiles "list"

getList :: ListOptions -> Annex [(UUID, RemoteName, TrustLevel)]
getList o
	| allRepos o = nubBy ((==) `on` fst3) <$> ((++) <$> getRemotes <*> getAllUUIDs)
	| otherwise = getRemotes
  where
	getRemotes = do
		rs <- remoteList
		ts <- mapM (lookupTrust . uuid) rs
		hereu <- getUUID
		heretrust <- lookupTrust hereu
		let l = (hereu, "here", heretrust) : zip3 (map uuid rs) (map name rs) ts
		return $ filter (\(_, _, t) -> t /= DeadTrusted) l 
	getAllUUIDs = do
		rs <- M.toList <$> uuidDescMap
		rs3 <- forM rs $ \(u, d) -> (,,)
			<$> pure u
			<*> pure (fromUUIDDesc d)
			<*> lookupTrust u
		return $ sortBy (comparing snd3) $
			filter (\t -> thd3 t /= DeadTrusted) rs3

printHeader :: [(UUID, RemoteName, TrustLevel)] -> Annex ()
printHeader l = liftIO $ putStrLn $ safeOutput $ lheader $ map (\(_, n, t) -> (n, t)) l

start :: [(UUID, RemoteName, TrustLevel)] -> SeekInput -> OsPath -> Key -> CommandStart
start l _si file key = do
	ls <- S.fromList <$> keyLocations key
	qp <- coreQuotePath <$> Annex.getGitConfig
	liftIO $ B8.putStrLn $ quote qp $
		format (map (\(u, _, t) -> (t, S.member u ls)) l) file
	stop

type Present = Bool

lheader :: [(RemoteName, TrustLevel)] -> String
lheader remotes = unlines (zipWith formatheader [0..] remotes) ++ pipes (length remotes)
  where
	formatheader n (remotename, trustlevel) = pipes n ++ remotename ++ trust trustlevel
	pipes = flip replicate '|'
	trust UnTrusted = " (untrusted)"
	trust _ = ""

format :: [(TrustLevel, Present)] -> OsPath -> StringContainingQuotedPath
format remotes file = UnquotedString (thereMap) <> " " <> QuotedPath file
  where 
	thereMap = concatMap there remotes
	there (UnTrusted, True) = "x"
	there (_, True) = "X"
	there (_, False) = "_"
