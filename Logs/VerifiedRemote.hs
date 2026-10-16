{- git-annex verified remote log
 - 
 - This is similar to the git-annex remote log, but only remote configs
 - that have been verified by the user to be safe to use.
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 - 
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TupleSections #-}

module Logs.VerifiedRemote (
	remoteConfigMap,
	readRemoteLog,
	configSet,
	configUpdate,
	omitUnverifiableFields,
	addUnverifiableFields,
	VerifyingRemoteConfigId(..),
	UnverifiedRemoteAmelioration(..),
	unverifiedRemoteAmelioration,
	readVerifyingRemoteLog,
	storeVerifyingRemoteLog,
	removeVerifyingLog
) where

import Annex.Common
import qualified Annex
import Types.Remote
import Types.VectorClock
import Logs.File
import Logs.UUIDBased
import Logs.Remote.Pure
import Logs.MapLog
import Annex.SpecialRemote.Config
import qualified Utility.FileIO as F

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

configSet :: UUID -> RemoteConfig -> Annex ()
configSet u c = configSetMulti [(u, c)]

{- Only sets the RemoteConfig if the log already contains a RemoteConfig
 - for this UUID. -}
configUpdate :: UUID -> RemoteConfig -> Annex ()
configUpdate u c = configSetMulti' (M.member u . fromMapLog) [(u, c)]

configSetMulti :: [(UUID, RemoteConfig)] -> Annex ()
configSetMulti = configSetMulti' (const True)

configSetMulti'
	:: (Log RemoteConfig -> Bool)
	-> [(UUID, RemoteConfig)]
	-> Annex ()
configSetMulti' shouldset ucfgs = do
	c <- currentVectorClock
	logf <- fromRepo gitAnnexVerifiedRemoteLog
	lck <- fromRepo gitAnnexVerifiedRemoteLock
	old <- parseRemoteConfigLog . fromMaybe mempty
		<$> readLogFile logf lck
	writeLogFile' logf $ \tmp -> F.writeFile tmp $
		toLazyByteString $ buildRemoteConfigLog $
			foldl' (accumchange c) old ucfgs
	Annex.changeState $ \s ->
		s { Annex.verifiedremoteconfigmap = Nothing }
  where
	accumchange c old (u, cfg)
		| shouldset old =
			changeLog c u (omitUnverifiableFields cfg) old
		| otherwise = old

{- Hidden encryption fields cannot be verified by the user, and in any
 - case do not configure the location that the remote accesses, and so
 - are omitted from this log file.
 -}
omitUnverifiableFields :: RemoteConfig -> RemoteConfig
omitUnverifiableFields c = M.withoutKeys c hiddenEncryptionFields

addUnverifiableFields
	:: M.Map UUID RemoteConfig
	-> M.Map UUID RemoteConfig
	-> M.Map UUID RemoteConfig
addUnverifiableFields unverifiedconfigs verifiedconfigs =
	M.mapWithKey go verifiedconfigs
  where
	go u verifiedconfig = case M.lookup u unverifiedconfigs of
		Just unverifiedconfig ->
			let unverifiable = M.restrictKeys
				unverifiedconfig
				hiddenEncryptionFields
			in M.union unverifiable verifiedconfig
		Nothing -> verifiedconfig

{- Map of remotes by uuid with their RemoteConfigs. Cached for speed.
 -
 - Nothing when the verified remote config log file does not exist yet.
 -}
remoteConfigMap :: Annex (Maybe (M.Map UUID RemoteConfig))
remoteConfigMap =
	maybe load return =<< Annex.getState Annex.verifiedremoteconfigmap
  where
	load = do
		m <- readRemoteLog
		Annex.changeState $ \s ->
			s { Annex.verifiedremoteconfigmap = Just m }
		return m

readRemoteLog :: Annex (Maybe (M.Map UUID RemoteConfig))
readRemoteLog = do
	logf <- fromRepo gitAnnexVerifiedRemoteLog
	lck <- fromRepo gitAnnexVerifiedRemoteLock
	fmap calcRemoteConfigMap <$> readLogFile logf lck

data UnverifiedRemoteAmelioration = UnverifiedRemoteAmelioration
	{ preventGitPush :: Maybe String
	-- ^ Prevent git push to all remotes.
	, preventAnnexWrite :: Maybe String
	-- ^ Prevent any git-annex object modification of all remotes.
	, preventAnnexAccess :: UUID -> Maybe String
	-- ^ Prevent any git-annex object access to remotes whose UUID
	-- the function selects. The returned message explains why.
	}

{- Given a list of UUIDs of all currently enabled remotes, check if any of
 - them are not in the verified remote log, and depending on their
 - remote.name.unverified configuration, returns the necessary
 - amelioration to prevent exfiltation of data from an unverified remote
 - to an attacker controlled remote.
 -}
unverifiedRemoteAmelioration
	:: [(UUID, RemoteGitConfig, Maybe RemoteName)]
	-> (M.Map UUID RemoteConfig)
	-- ^ Logs.Remote.remoteConfigMap
	-> Annex (Maybe UnverifiedRemoteAmelioration)
unverifiedRemoteAmelioration rs configmap = remoteConfigMap >>= \case
	Just m -> return $ 
		let l = mapMaybe (notinmap m) rs
		in Just $ UnverifiedRemoteAmelioration
			{ preventGitPush = 
				checkprevented pushblocker l "Push"
			, preventAnnexWrite =
				checkprevented writeblocker l "Write"
			, preventAnnexAccess =
				accessblocker l
			}

	-- The log file does not exist yet. This is either a newly
	-- initialized repository, or git-annex has been upgraded from a
	-- version that did not use this log.
	--
	-- Handle upgrade by populating the log with the
	-- current configs of all the currently enabled remotes.
	--
	-- In a newly initialized repository, there are not yet
	-- any enabled remotes, and this creates an empty log file.
	Nothing -> do
		configSetMulti $ mapMaybe (copyfromconfigmap configmap) rs
		return Nothing
  where
	-- Any special remote that is unverified prevents git push to all
	-- remotes.
	pushblocker (_, gc, remotename)
		| remoteAnnexVerified gc /= RemoteConfigVerified = remotename
		| otherwise = Nothing

	-- Any autoenabled special remote that is unverified prevents
	-- annex object write to all remotes.
	writeblocker (_, gc, remotename)
		| remoteAnnexVerified gc == RemoteConfigUnverifiedAutoEnabled = remotename
		| remoteAnnexVerified gc == RemoteConfigUnverified = remotename
		| otherwise = Nothing

	-- Prevent annex object access to any special remote that was
	-- enabled manually but is not yet verified.
	accessblocker l u = case filter (\(u',_, _) -> u' == u) l of
		((_, gc, Just remotename):_)
			| remoteAnnexVerified gc == RemoteConfigUnverifiedManualEnabled ->
				preventedunverified remotename
			| remoteAnnexVerified gc == RemoteConfigUnverified ->
				preventedunverified remotename
		_ -> Nothing
	
	prevented remotename msg = Just $ unwords
		[ msg
		, "Run this command to verify it:"
		, "git-annex verifyremote"
		, remotename
		]

	preventdueto remotetoverify action =
		prevented remotetoverify $ unwords
			[ action
			, "prevented due to an unverified special"
			, "remote."
			]
	
	preventedunverified remotename = 
		prevented remotename $
			"This special remote cannot be used since " ++ 
				"it's not verified."

	checkprevented blocker l action =
		case mapMaybe blocker l of
			[] -> Nothing
			(remotename:_) -> preventdueto remotename action
	
	notinmap m i@(u, gc, _)
		| M.notMember u' configmap = Nothing
		| M.member u' m = Nothing
		| otherwise = Just i
	  where
		u' = getcu u gc

	copyfromconfigmap m (u, gc, _) =
		(getcu u gc, ) <$> M.lookup u m
	
	getcu u gc = fromMaybe u (remoteAnnexConfigUUID gc)

data VerifyingRemoteConfigId = VerifyingRemoteConfigId
	{ fromVerifyingRemoteConfigId :: Integer }
	deriving (Eq, Ord)

readVerifyingRemoteLog :: Annex (M.Map VerifyingRemoteConfigId RemoteConfig)
readVerifyingRemoteLog = do
	logf <- fromRepo gitAnnexVerifyingRemoteLog
	lck <- fromRepo gitAnnexVerifyingRemoteLock
	simpleMap . parseVerifyingRemoteLog . fromMaybe mempty
		<$> readLogFile logf lck

storeVerifyingRemoteLog :: RemoteConfig -> Annex VerifyingRemoteConfigId
storeVerifyingRemoteLog cfg =
	modifyVerifyingRemoteLog $ \c n ->
		changeMapLog c n (omitUnverifiableFields cfg)

removeVerifyingLog :: VerifyingRemoteConfigId -> Annex ()
removeVerifyingLog n =
	void $ modifyVerifyingRemoteLog
		(\_ _ (MapLog m) -> MapLog (M.delete n m))

modifyVerifyingRemoteLog
	 :: (CandidateVectorClock -> VerifyingRemoteConfigId -> MapLog VerifyingRemoteConfigId RemoteConfig -> MapLog VerifyingRemoteConfigId RemoteConfig)
	 -> Annex VerifyingRemoteConfigId
modifyVerifyingRemoteLog f = do
	c <- currentVectorClock
	logf <- fromRepo gitAnnexVerifyingRemoteLog
	lck <- fromRepo gitAnnexVerifyingRemoteLock
	old <- parseVerifyingRemoteLog . fromMaybe mempty
		<$> readLogFile logf lck
	let nextcid = VerifyingRemoteConfigId $ succ $
		maybe 0 fromVerifyingRemoteConfigId $
		headMaybe $ reverse $ M.keys $ simpleMap old
	writeLogFile' logf $ \tmp -> F.writeFile tmp $
		toLazyByteString $ buildVerifyingRemoteLog (f c nextcid old)
	return nextcid

parseVerifyingRemoteLog
	:: L.ByteString
	-> MapLog VerifyingRemoteConfigId RemoteConfig
parseVerifyingRemoteLog =
	parseMapLog parseVerifyingRemoteConfigId remoteConfigParser

buildVerifyingRemoteLog
	:: MapLog VerifyingRemoteConfigId RemoteConfig
	-> Builder
buildVerifyingRemoteLog = 
	buildMapLog buildVerifyingRemoteConfigId buildRemoteConfig

parseVerifyingRemoteConfigId :: A8.Parser VerifyingRemoteConfigId
parseVerifyingRemoteConfigId = VerifyingRemoteConfigId <$> A8.decimal

buildVerifyingRemoteConfigId :: VerifyingRemoteConfigId -> Builder
buildVerifyingRemoteConfigId = integerDec . fromVerifyingRemoteConfigId
