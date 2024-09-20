{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Sim where

import Command
import Annex.Sim
import Annex.Sim.File
import Annex.Perms
import Utility.Env

import System.Random
import qualified Data.Map as M

cmd :: Command
cmd = command "sim" SectionTesting
	"simulate a network of repositories"
	paramCommand (withParams seek)

seek :: CmdParams -> CommandSeek
seek ("start":[]) = start Nothing
seek ("start":simfile:[]) = start (Just simfile)
seek ("end":[]) = do
	simdir <- fromRawFilePath <$> fromRepo gitAnnexSimDir
	whenM (liftIO $ doesDirectoryExist simdir) $ do
		liftIO $ removeDirectoryRecursive simdir
	showLongNote $ UnquotedString "Sim ended."
seek ("visit":reponame:[]) = do
	simdir <- fromRepo gitAnnexSimDir
	liftIO (restoreSim simdir) >>= \case
		Left err -> giveup err
		Right st -> case M.lookup (RepoName reponame) (simRepos st) of
			Just u -> do
				let dir = simRepoDirectory st u
				unlessM (liftIO $ doesDirectoryExist dir) $
					giveup "Simulated repository unavailable."
				showLongNote "Starting a shell in the simulated repository."
				shellcmd <- liftIO $ fromMaybe "sh" <$> getEnv "SHELL"
				exitcode <- liftIO $
					safeSystem' shellcmd []
						(\p -> p { cwd = Just dir })
				showLongNote "Finished visit to simulated repository."
				liftIO $ exitWith exitcode
			Nothing -> giveup $ unwords
				[ "There is no simulated repository with that name."
				, "Choose from:" 
				, unwords $ map fromRepoName $ M.keys (simRepos st)
				]
seek ("show":[]) = do
	simdir <- fromRepo gitAnnexSimDir
	liftIO (restoreSim simdir) >>= \case
		Left err -> giveup err
		Right st -> liftIO $ putStr $ generateSimFile $
			reverse $ simHistory st
seek ps = case parseSimCommand ps of
	Left err -> giveup err
	Right simcmd -> do
		repobyname <- mkGetExistingRepoByName
		simdir <- fromRepo gitAnnexSimDir
		liftIO (restoreSim simdir) >>= \case
			Left err -> giveup err
			Right st -> 
				runSimCommand simcmd repobyname st
					>>= liftIO . suspendSim

start :: Maybe FilePath -> CommandSeek
start simfile = do
	simdir <- fromRawFilePath <$> fromRepo gitAnnexSimDir
	whenM (liftIO $ doesDirectoryExist simdir) $
		giveup "A sim was previously started. Use `git-annex sim end` to stop it before starting a new one."
	
	rng <- fst . random <$> initStdGen
	let st = emptySimState rng simdir
	case simfile of
		Nothing -> startup simdir st []
		Just f -> liftIO (readFile f) >>= \c -> 
			case parseSimFile c of
				Left err -> giveup err
				Right cs -> startup simdir st cs
	showLongNote $ UnquotedString "Sim started."
  where
	startup simdir st cs = do
		repobyname <- mkGetExistingRepoByName
		createAnnexDirectory (toRawFilePath simdir)
		let st' = recordSeed st cs
		st'' <- go st' repobyname cs
		liftIO $ suspendSim st''

	go st _ [] = return st
	go st repobyname (c:cs) = do
		st' <- runSimCommand c repobyname st
		go st' repobyname cs
