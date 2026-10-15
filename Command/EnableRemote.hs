{- git-annex command
 -
 - Copyright 2013-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.EnableRemote where

import Command
import qualified Annex
import qualified Logs.Remote
import qualified Logs.VerifiedRemote
import qualified Types.Remote as R
import qualified Git
import qualified Annex.SpecialRemote as SpecialRemote
import qualified Remote
import qualified Remote.Git
import qualified Command.InitRemote
import Logs.UUID
import Annex.UUID
import Annex.Hook
import Config
import Config.DynamicConfig
import Types.GitConfig
import Types.ProposedAccepted
import Command.VerifyRemote (remoteVerificationMessage)

import qualified Data.Map as M

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "enableremote" SectionSetup
		"enables git-annex to use a remote"
		(paramPair paramName $ paramOptional $ paramRepeating paramParamValue)
		(seek <$$> optParser)

data EnableRemoteOptions = EnableRemoteOptions
	{ cmdparams :: CmdParams
	, withUrl :: Bool
	}

optParser :: CmdParamsDesc -> Parser EnableRemoteOptions
optParser desc = EnableRemoteOptions
	<$> cmdParams desc
	<*> switch
		( long "with-url"
		<> short 'u'
		<> help "configure remote with an annex:: url"
		)

seek :: EnableRemoteOptions -> CommandSeek
seek o = withWords (commandAction . (start o)) (cmdparams o)

start :: EnableRemoteOptions -> [String] -> CommandStart
start _ [] = unknownNameError "Specify the remote to enable."
start o (name:rest) = go =<< filter matchingname <$> Annex.getGitRemotes
  where
	matchingname r = Git.remoteName r == Just name
	go [] = deadLast name $ 
		startSpecialRemote o name $
			Logs.Remote.keyValToConfig Proposed rest
	go (r:_)
		| not (null rest) = go []
		| otherwise = do
			-- This could be either a normal git remote or a special
			-- remote that has an url (eg gcrypt).
			rs <- Remote.remoteList
			case filter (\rmt -> Remote.name rmt == name) rs of
				(rmt:_) | R.remotetype rmt == Remote.Git.remote ->
					startNormalRemote name r
				_  -> go []

-- enableremote of a normal git remote with no added parameters is a special case
-- that retries probing the remote uuid.
startNormalRemote :: RemoteName -> Git.Repo -> CommandStart
startNormalRemote name r = starting "enableremote (normal)" ai si $ do
	setRemoteIgnore r False
	r' <- Remote.Git.configRead False r
	u <- getRepoUUID r'
	next $ return $ u /= NoUUID
  where
	ai = ActionItemOther (Just (UnquotedString name))
	si = SeekInput [name]

startSpecialRemote :: EnableRemoteOptions -> RemoteName -> R.RemoteConfig -> [(UUID, R.RemoteConfig, Maybe (SpecialRemote.ConfigFrom UUID))] -> CommandStart
startSpecialRemote o = startSpecialRemote' "enableremote" (performSpecialRemote o)

type PerformSpecialRemote = RemoteType -> UUID -> RemoteName -> R.RemoteConfig -> R.RemoteConfig -> RemoteGitConfig -> Maybe (SpecialRemote.ConfigFrom UUID) -> CommandPerform

startSpecialRemote' :: String -> PerformSpecialRemote -> RemoteName -> R.RemoteConfig -> [(UUID, R.RemoteConfig, Maybe (SpecialRemote.ConfigFrom UUID))] -> CommandStart
startSpecialRemote' cname perform name config [] = do
	m <- SpecialRemote.specialRemoteMap
	confm <- Logs.Remote.remoteConfigMap
	Remote.nameToUUID' name >>= \case
		([u], _) | u `M.member` m ->
			startSpecialRemote' cname perform name config $
				[(u, fromMaybe M.empty (M.lookup u confm), Nothing)]
		(_, msg) -> unknownNameError msg
startSpecialRemote' cname perform name config ((u, c, mcu):[]) =
	starting cname ai si $ do
		let fullconfig = config `M.union` c	
		t <- either giveup return (SpecialRemote.findType fullconfig)
		case SpecialRemote.findType c of
			Right t' | t' /= t ->
				giveup "Cannot change type= of existing special remote. Instead, use: git-annex initremote --sameas"
			_ -> noop
		gc <- maybe (liftIO dummyRemoteGitConfig) 
			(return . R.gitconfig)
			=<< Remote.byUUID u
		perform t u name c fullconfig gc mcu
  where
	ai = ActionItemOther (Just (UnquotedString name))
	si = SeekInput [name]
startSpecialRemote' _ _ _ _ _ =
	giveup "Multiple remotes have that name. Either use git-annex renameremote to rename them, or specify the uuid of the remote."

performSpecialRemote :: EnableRemoteOptions -> PerformSpecialRemote
performSpecialRemote o t u name oldc c gc mcu = do
	rs <- Remote.remoteList
	prevrmt <- case filter (\rmt -> Remote.name rmt == name) rs of
		(rmt:_) -> return (Just rmt)
		_ -> return Nothing
	unless (maybe u Remote.uuid prevrmt == u) $
		giveup $ "Not overwriting currently configured git remote named \"" ++ name ++ "\""
	enablePrePushHook >>= \case
		Nothing -> noop
		Just msg -> giveup $ "Cannot enable this special remote because " ++ msg
	(c', u') <- R.setup t (R.Enable oldc) (Just u) name Nothing c gc
	next $ cleanupSpecialRemote o t u' c' mcu prevrmt name

cleanupSpecialRemote :: EnableRemoteOptions -> RemoteType -> UUID -> R.RemoteConfig -> Maybe (SpecialRemote.ConfigFrom UUID) -> Maybe Remote -> RemoteName -> CommandCleanup
cleanupSpecialRemote o t u c mcu prevrmt name = do
	cu <- case mcu of
		Nothing -> do
			Logs.Remote.configSet u c
			return u
		Just (SpecialRemote.ConfigFrom cu) -> do
			setRemoteConfigUUID c cu
			Logs.Remote.configSet cu c
			return cu
	when (isJust prevrmt) $
		Logs.VerifiedRemote.configUpdate cu c
	remoteVerificationMessage name cu c >>= \case
		Just msg -> showLongNote (UnquotedString msg)
		Nothing -> noop
	Remote.byUUID u >>= \case
		Nothing -> noop
		Just r -> do
			repo <- R.getRepo r
			setRemoteIgnore repo False
	when (withUrl o) $
		Command.InitRemote.setAnnexUrl c
	unless (Remote.gitSyncableRemoteType t || withUrl o) $
		setRemoteSkipFetchAll c True
	return True

unknownNameError :: String -> Annex a
unknownNameError prefix = do
	m <- SpecialRemote.specialRemoteMap
	descm <- M.unionWith Remote.addName
		<$> uuidDescMap
		<*> pure (M.map toUUIDDesc m)
	specialmsg <- if M.null m
			then pure "(No special remotes are currently known; perhaps use initremote instead?)"
			else Remote.prettyPrintUUIDsDescs
				"known special remotes"
				descm (M.keys m)
	disabledremotes <- filterM isdisabled =<< Annex.getGitRemotes
	let remotesmsg = unlines $ map ("\t" ++) $
		mapMaybe Git.remoteName disabledremotes
	giveup $ concat $ filter (not . null) [prefix ++ "\n", remotesmsg, specialmsg]
  where
	isdisabled r = anyM id
		[ (==) NoUUID <$> getRepoUUID r
		, liftIO . getDynamicConfig . remoteAnnexIgnore
			=<< Annex.getRemoteGitConfig r
		]

-- Use dead remote only when there is no other remote
-- with the same name
deadLast :: RemoteName -> ([(UUID, R.RemoteConfig, Maybe (SpecialRemote.ConfigFrom UUID))] -> Annex a) -> Annex a
deadLast name use =
	SpecialRemote.findExisting' name >>= \case
		([], l) -> use l
		(l, _) -> use l
