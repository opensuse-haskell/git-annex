{- git-annex command
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Map where

import Command
import qualified Git
import qualified Git.Url
import qualified Git.Config
import qualified Git.Construct
import qualified Remote
import qualified Annex
import Annex.Ssh
import Annex.UUID
import Logs.UUID
import Logs.Trust
import Types.TrustLevel
import qualified Remote.Helper.Ssh as Ssh
import qualified Utility.Dot as Dot
import qualified Messages.JSON as JSON
import Messages.JSON ((.=))
import Utility.Aeson (packString)

import qualified Data.Map as M

-- a repo and its remotes
type RepoRemotes = (Git.Repo, [Git.Repo])

cmd :: Command
cmd = dontCheck repoExists $ withAnnexOptions [jsonOptions] $
	command "map" SectionQuery
		"generate map of repositories"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = startingNoMessage (ActionItemOther Nothing) $ do
	rs <- combineSame <$> (spider =<< gitRepo)

	umap <- uuidDescMap
	trustmap <- trustMapLoad
	
	ifM (outputJSONMap rs trustmap umap)
		( next $ return True
		, do
			file <- (</>)
				<$> fromRepo gitAnnexDir
				<*> pure (literalOsPath "map.dot")

			liftIO $ writeFile (fromOsPath file) (drawMap rs trustmap umap)
			next $
				ifM (Annex.getRead Annex.fast)
					( runViewer file []
					, runViewer file
			 			[ ("xdot", [File (fromOsPath file)])
						, ("dot", [Param "-Tx11", File (fromOsPath file)])
						]	
					)
		)

runViewer :: OsPath -> [(String, [CommandParam])] -> Annex Bool
runViewer file [] = do
	showLongNote $ UnquotedString $ "left map in " ++ fromOsPath file
	return True
runViewer file ((c, ps):rest) = ifM (liftIO $ inSearchPath c)
	( do
		showLongNote $ UnquotedString $ "running: " ++ c ++ " " ++ unwords (toCommand ps)
		showOutput
		liftIO $ boolSystem c ps
	, runViewer file rest
	)

{- Generates a graph for dot(1). Each repository, and any other uuids
 - (except for dead ones), are displayed as a node, and each of its
 - remotes is represented as an edge pointing at the node for the remote.
 -
 - The order nodes are added to the graph matters, since dot will draw
 - the first ones near to the top and left. So it looks better to put
 - the repositories first, followed by uuids that were not matched
 - to a repository.
 -}
drawMap :: [RepoRemotes] -> TrustMap -> UUIDDescMap -> String
drawMap rs trustmap umap = Dot.graph $ repos ++ others
  where
	repos = map (node umap (map fst rs) trustmap) rs
	ruuids = map (getUncachedUUID . fst) rs
	others = map uuidnode $ 
		filter (\u -> M.lookup u trustmap /= Just DeadTrusted) $
		filter (`notElem` ruuids) (M.keys umap)
	uuidnode u = trustDecorate trustmap u $
		Dot.graphNode
			(fromUUID u)
			(fromUUIDDesc $ M.findWithDefault mempty u umap)

hostname :: Git.Repo -> String
hostname r
	| Git.repoIsUrl r = fromMaybe (Git.repoLocation r) (Git.Url.host r)
	| otherwise = "localhost"

basehostname :: Git.Repo -> String
basehostname r = fromMaybe "" $ headMaybe $ splitc '.' $ hostname r

{- A description to display for a repo. Uses the description 
 - from uuid.log if available, or the remote name if not. -}
repoDesc :: UUIDDescMap -> Git.Repo -> String
repoDesc umap r
	| repouuid == NoUUID = fallback
	| otherwise = maybe fallback fromUUIDDesc $ M.lookup repouuid umap
  where
	repouuid = getUncachedUUID r
	fallback = fromMaybe "unknown" $ Git.remoteName r

{- A unique id for the node for a repo. Uses the annex.uuid if available. -}
nodeId :: Git.Repo -> String
nodeId r =
	case getUncachedUUID r of
		NoUUID -> Git.repoLocation r
		u@(UUID _) -> fromUUID u

{- A node representing a repo. -}
node :: UUIDDescMap -> [Git.Repo] -> TrustMap -> RepoRemotes -> String
node umap fullinfo trustmap (r, rs) = unlines $ n:edges
  where
	n = Dot.subGraph (hostname r) (basehostname r) "lightblue" $
		trustDecorate trustmap (getUncachedUUID r) $
			Dot.graphNode (nodeId r) (repoDesc umap r)
	edges = map (edge umap fullinfo r) rs

{- An edge between two repos. The second repo is a remote of the first. -}
edge :: UUIDDescMap -> [Git.Repo] -> Git.Repo -> Git.Repo -> String	
edge umap fullinfo from to =
	Dot.graphEdge (nodeId from) (nodeId fullto) edgename
  where
	-- get the full info for the remote, to get its UUID
	fullto = findfullinfo to
	findfullinfo n =
		case filter (same n) fullinfo of
			[] -> n
			(n':_) -> n'
	{- Only name an edge if the name is different than the name
	 - that will be used for the destination node, and is
	 - different from its hostname. (This reduces visual clutter.) -}
	edgename = maybe Nothing calcname $ Git.remoteName to
	calcname n
		| n `elem` [repoDesc umap fullto, hostname fullto] = Nothing
		| otherwise = Just n

trustDecorate :: TrustMap -> UUID -> String -> String
trustDecorate trustmap u s = case M.lookup u trustmap of
	Just Trusted -> Dot.fillColor "green" s
	Just UnTrusted -> Dot.fillColor "red" s
	Just SemiTrusted -> Dot.fillColor "white" s
	Just DeadTrusted -> Dot.fillColor "grey" s
	Nothing -> Dot.fillColor "white" s

{- Recursively searches out remotes starting with the specified local repo. -}
spider :: Git.Repo -> Annex [RepoRemotes]
spider r = spider' [r] []
spider' :: [Git.Repo] -> [RepoRemotes] -> Annex [RepoRemotes]
spider' [] known = return known
spider' (r:rs) known
	| any (same r) (map fst known) = spider' rs known
	| otherwise = do
		r' <- scan r

		-- The remotes will be relative to r', and need to be
		-- made absolute for later use.
		remotes <- mapM (absRepo r') =<<
			if Git.repoIsUrl r
				then liftIO $ Git.Construct.fromRemoteUrlRemotes r'
				else liftIO $ Git.Construct.fromRemotes r'

		spider' (rs ++ remotes) ((r', remotes):known)

{- Converts repos to a common absolute form. -}
absRepo :: Git.Repo -> Git.Repo -> Annex Git.Repo
absRepo reference r
	| Git.repoIsUrl reference = return $
		Git.Construct.localToUrl reference r
	| Git.repoIsUrl r = return r
	| otherwise = liftIO $ do
		r' <- Git.Construct.fromPath =<< absPath (Git.repoPath r)
		r'' <- safely $ flip Annex.eval Annex.gitRepo =<< Annex.new r'
		return $ (fromMaybe r' r'')
			{ Git.remoteName = Git.remoteName r
			}

{- Checks if two repos are the same. -}
same :: Git.Repo -> Git.Repo -> Bool
same a b
	| both Git.repoIsUrl = matching Git.Url.scheme && matching Git.Url.authority && matching Git.repoPath
	| neither Git.repoIsUrl = matching Git.repoPath
	| otherwise = False
  where
	matching t = t a == t b
	both t = t a && t b
	neither t = not (t a) && not (t b)

{- reads the config of a remote, with progress display -}
scan :: Git.Repo -> Annex Git.Repo
scan r = do
	unlessM jsonOutputEnabled $
		showStartMessage (StartMessage "map" (ActionItemOther (Just $ UnquotedString $ Git.repoDescribe r)) (SeekInput []))
	v <- tryScan r
	case v of
		Just r' -> do
			showEndOk
			return r'
		Nothing -> do
			showOutput
			showEndFail
			return r

{- tries to read the config of a remote, returning it only if it can
 - be accessed -}
tryScan :: Git.Repo -> Annex (Maybe Git.Repo)
tryScan r
	| Git.repoIsSsh r = sshscan
	| Git.repoIsUrl r = case Git.remoteName r of
		-- Can't scan a non-ssh url, so use any cached uuid for it.
		Just n -> Just <$> (either
			(const (pure r))
			(liftIO . setUUID r . Remote.uuid)
			=<< Remote.byName' n)
		Nothing -> return $ Just r
	| otherwise = liftIO $ safely $ Git.Config.read r
  where
	pipedconfig st pcmd params = liftIO $ safely $
		withCreateProcess p (pipedconfig' st p)
	  where
		p = (proc pcmd $ toCommand params)
			{ std_out = CreatePipe }

	pipedconfig' st p _ (Just h) _ pid = 
		forceSuccessProcess p pid
			`after`
		Git.Config.hRead r st h
	pipedconfig' _ _ _ _ _ _ = error "internal"

	configlist = Ssh.onRemote NoConsumeStdin r
		(pipedconfig Git.Config.ConfigList, return Nothing) "configlist" [] []
	manualconfiglist = do
		gc <- Annex.getRemoteGitConfig r
		(sshcmd, sshparams) <- Ssh.toRepo NoConsumeStdin r gc remotecmd
		liftIO $ pipedconfig Git.Config.ConfigNullList sshcmd sshparams
	  where
		remotecmd = "sh -c " ++ shellEscape
			(cddir ++ " && " ++ "git config --null --list")
		dir = fromOsPath $ Git.repoPath r
		cddir
			| "/~" `isPrefixOf` dir =
				let (userhome, reldir) = span (/= '/') (drop 1 dir)
				in "cd " ++ userhome ++ " && " ++ cdto (drop 1 reldir)
			| otherwise = cdto dir
		cdto p = "if ! cd " ++ shellEscape p ++ " 2>/dev/null; then cd " ++ shellEscape p ++ ".git; fi"

	-- First, try sshing and running git config manually,
	-- only fall back to git-annex-shell configlist if that
	-- fails.
	-- 
	-- This is done for two reasons, first I'd like this
	-- subcommand to be usable on non-git-annex repos.
	-- Secondly, configlist doesn't include information about
	-- the remote's remotes.
	sshscan = do
		sshnote
		v <- manualconfiglist
		case v of
			Nothing -> do
				sshnote
				configlist
			ok -> return ok

	sshnote = unlessM jsonOutputEnabled $ do
		showAction "sshing"
		showOutput

{- Spidering can find multiple paths to the same repo, so this is used
 - to combine (really remove) duplicate repos with the same UUID. -}
combineSame :: [RepoRemotes] -> [RepoRemotes]
combineSame = map snd . nubBy sameuuid . map pair
  where
	sameuuid (u1, _) (u2, _) = u1 == u2 && u1 /= NoUUID
	pair (r, rs) = (getUncachedUUID r, (r, rs))

safely :: IO Git.Repo -> IO (Maybe Git.Repo)
safely a = do
	result <- tryNonAsync a
	case result of
		Left _ -> return Nothing
		Right r' -> return $ Just r'

outputJSONMap :: [RepoRemotes] -> TrustMap -> UUIDDescMap -> Annex Bool
outputJSONMap rs trustmap umap = 
	showFullJSON $ JSON.AesonObject $ case mapo of
		JSON.Object obj -> obj
		_ -> error "internal"
  where
	mapo = JSON.object
		[ "nodes" .= map mknode (filterdead fst rs)
		]
	
	mknode (r, remotes) = JSON.object
		[ "description" .= packString (repoDesc umap r)
		, "uuid" .= mkuuid (getUncachedUUID r)
		, "url" .= packString (Git.repoLocation r)
		, "remotes" .= map mkremote (filterdead id remotes)
		]
	
	mkremote r = JSON.object
		[ "remote" .= (packString <$> Git.remoteName r)
		, "uuid" .= mkuuid (getUncachedUUID r)
		, "url" .= packString (Git.repoLocation r)
		]
	
	mkuuid NoUUID = Nothing
	mkuuid u = Just $ packString $ fromUUID u
	
	filterdead f = filter
		(\i -> M.lookup (getUncachedUUID (f i)) trustmap /= Just DeadTrusted)

