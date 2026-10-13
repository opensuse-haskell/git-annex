{- git-annex command
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.VerifyRemote where

import Command
import qualified Logs.Remote
import qualified Logs.VerifiedRemote
import qualified Remote
import Types.RemoteConfig
import Types.ProposedAccepted
import Annex.SpecialRemote.Config
import Utility.Hash

import qualified Data.Map as M
import qualified Data.Set as S

cmd :: Command
cmd = command "verifyremote" SectionSetup
	"verify if a special remote is safe to use"
	paramName
	(seek <$$> optParser)

data VerifyRemoteOptions = VerifyRemoteOptions
	{ remote :: DeferredParse Remote
	, safeunsafe :: Maybe SafeUnsafe
	}

data SafeUnsafe = Safe RemoteConfigId | Unsafe

data RemoteConfigId = RemoteConfigId { fromRemoteConfigId :: String }
	deriving (Eq)

optParser :: CmdParamsDesc -> Parser VerifyRemoteOptions
optParser _ = VerifyRemoteOptions
	<$> (mkParseRemoteOption <$> remotearg)
	<*> optional safeUnsafeParser
  where
	remotearg = strArgument
		( metavar paramRemote
		<> help "remote to verify"
		<> completeRemotes
		)

safeUnsafeParser :: Parser SafeUnsafe
safeUnsafeParser = safe <|> unsafe
  where
	safe = option (Safe . RemoteConfigId <$> str)
		( long "safe" <> metavar "ID"
		<> help "special remote configuration is safe"
		)
	unsafe = flag' Unsafe
		( long "unsafe"
		<> help "special remote configuration is unsafe"
		)

seek :: VerifyRemoteOptions -> CommandSeek
seek = commandAction . start

start :: VerifyRemoteOptions -> CommandStart
start o = do
	r <- getParsed (remote o)
	let si = SeekInput [Remote.name r]
	let ai = ActionItemUUID
		(Remote.uuid r)
		(UnquotedString (Remote.name r))
	case safeunsafe o of
		Just (Safe cid) -> starting "verifyremote" ai si $
			performVerifySafe cid r
		Just Unsafe -> starting "verifyremote" ai si $
			performVerifyUnsafe r
		Nothing -> startVerify r si

startVerify :: Remote -> SeekInput -> CommandStart
startVerify r si = do
	showCustom "verifyremote" si $ do
		remoteconfig <- getUnverifiedRemoteConfig (Remote.uuid r)
		verifiedremoteconfig <- getVerifiedRemoteConfig (Remote.uuid r)
		msg <- case remoteVerificationMessage' (Remote.name r) remoteconfig verifiedremoteconfig of
			Just msg -> return msg
			Nothing -> if verifiedremoteconfig == remoteconfig
				then return "There have been no changes to the remote's configuration since it was last verified."
				else do
					-- Store new config as verified so that any
					-- changes to the omitfields take effect.
					Logs.VerifiedRemote.configSet (Remote.uuid r) remoteconfig
					return "No verification is necessary; updated the remote's configuration."			
		showRaw (encodeBS msg)
		return True
	stop

remoteVerificationMessage :: RemoteName -> UUID -> RemoteConfig -> Annex (Maybe String)
remoteVerificationMessage remotename u remoteconfig = 
	remoteVerificationMessage' remotename remoteconfig
		<$> getVerifiedRemoteConfig u

remoteVerificationMessage' :: RemoteName -> RemoteConfig -> RemoteConfig -> Maybe String
remoteVerificationMessage' remotename remoteconfig verifiedremoteconfig
	| visiblefields verifiedremoteconfig == visibleremoteconfig = Nothing
	| otherwise = Just $
		if verifiedremoteconfig /= mempty
			then updatemessage
			else newmessage
  where
	newmessage = unlines
		[ "This is the configuration of the remote:"
		, "\t" ++ displayRemoteConfig visibleremoteconfig
		, ""
		, "Is this a location that you expected to be using?"
		, "If so, run:  git-annex verifyremote "
			 ++ remotename ++ " --safe "
			 ++ fromRemoteConfigId cid
		, "If not, run: git-annex verifyremote "
			 ++ remotename ++ " --unsafe"
		]
	
	updatemessage = unlines
		[ "There is an updated configuration of the remote:"
		, "\t" ++ displayRemoteConfig visibleremoteconfig
		, ""
		, "Is this a location that you expected to be using?"
		, "If so, run:  git-annex verifyremote "
			 ++ remotename ++ " --safe "
			 ++ fromRemoteConfigId cid
		, "If not, you do not need to do anything; the previously"
		, "verified configuration of the remote will continue to"
		, "be used."
		]
	
	cid = genRemoteConfigId remoteconfig
	
	visibleremoteconfig = visiblefields remoteconfig

	visiblefields = flip M.withoutKeys omitfields

	omitfields = hiddenEncryptionFields <> S.fromList
		[ nameField
		, sameasNameField
		, sameasUUIDField
		]

performVerifySafe :: RemoteConfigId -> Remote -> CommandPerform
performVerifySafe cid r = do
	remoteconfig <- getUnverifiedRemoteConfig (Remote.uuid r)
	if genRemoteConfigId remoteconfig == cid
		then do
			Logs.VerifiedRemote.configSet (Remote.uuid r) remoteconfig
			next $ return True
		else do
			warning "invalid ID"
			next $ return False

performVerifyUnsafe :: Remote -> CommandPerform
performVerifyUnsafe r = do
	m <- Logs.VerifiedRemote.readRemoteLog
	let hasverifiedconfig = M.member (Remote.uuid r) m
	warning $ UnquotedString $ unlines $ 
		warningVerifyUnsafe (Remote.name r) hasverifiedconfig
	next $ return False

warningVerifyUnsafe :: RemoteName -> Bool -> [String]
warningVerifyUnsafe name True =
	[ "So the remote " ++ name ++ " is in a location you did"
	, "not expect to be using. You appear to be under attack."
	, ""
	, "No annexed files have been sent to that remote or any other"
	, "remote since it was enabled, and git pushes have also been"
	, "prevented."
	, ""
	, "But, annexed files may have been copied from that remote into"
	, "the local repository. There is a risk that those files could"
	, "be sent on to other remotes, or that information in the git-annex"
	, "branch could be pushed, leaking information about the content"
	, "of " ++ name ++ "."
	, ""
	, "Your options for dealing with this include:"
	, "* Identify who caused " ++ name ++ " to be configured this way,"
	, "  and remove any remotes under their control to avoid leaking data"
	, "  to them."
	, "* Delete this local git repository and all its contents."
	, "  A good choice if you know you will not lose any work or data."
	, ""
	, "As long as the remote " ++ name ++ " exists, annexed files"
	, "will continue to be blocked from being sent to any remote,"
	, "and git pushes will also be blocked. Once you have dealt"
	, "with this attack, you can unblock by running:"
	, "\tgit remote remove " ++ name
	]
warningVerifyUnsafe name False =
	[ "Unfortunately, information may have leaked about the content"
	, "of " ++ name ++ " since it was enabled by a previous version"
	, "of git-annex that did not verify the location of special remotes"
	, "before using them. You should assume that other remotes have"
	, "been sent the content of annexed files in " ++ name ++ "."
	]

getUnverifiedRemoteConfig :: UUID -> Annex RemoteConfig
getUnverifiedRemoteConfig u = do
	m <- Logs.Remote.readRemoteLog
	return $ fromMaybe mempty (M.lookup u m)

getVerifiedRemoteConfig :: UUID -> Annex RemoteConfig
getVerifiedRemoteConfig u = do
	m <- Logs.VerifiedRemote.readRemoteLog
	return $ fromMaybe mempty (M.lookup u m)

{- The RemoteConfig is hashed to generate the RemoteConfigId. This
 - ensures that the config does not change between the verification
 - display and marking the config as safe.
 -}
genRemoteConfigId :: RemoteConfig -> RemoteConfigId
genRemoteConfigId = RemoteConfigId
	. decodeBS
	. hashByteString
	. digestToHash
	. sha2_224 
	. encodeBL
	. concat
	. concatMap go
	. sort
	. M.toList
  where
	go (f, v) = [fromProposedAccepted f, fromProposedAccepted v]

displayRemoteConfig :: RemoteConfig -> String
displayRemoteConfig = intercalate " " 
	. map (\(f, v) -> fromProposedAccepted f ++ "=" ++ fromProposedAccepted v)
	. M.toList
