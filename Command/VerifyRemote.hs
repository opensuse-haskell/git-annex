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
import Logs.VerifiedRemote (VerifyingRemoteConfigId(..))
import qualified Remote
import qualified Types.Remote as Remote
import Types.RemoteConfig
import Types.ProposedAccepted
import Annex.SpecialRemote.Config

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

data SafeUnsafe = Safe VerifyingRemoteConfigId | Unsafe

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
	safe = option (Safe . VerifyingRemoteConfigId <$> maybeReader readish)
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
		remoteconfig <- getUnverifiedRemoteConfig (logUUID r)
		cid <- Logs.VerifiedRemote.storeVerifyingRemoteLog remoteconfig
		verifiedremoteconfig <- getVerifiedRemoteConfig (logUUID r)
		let msg = fromMaybe nochanges $
			remoteVerificationMessage' (Remote.name r) remoteconfig cid verifiedremoteconfig
		showRaw (encodeBS msg)
		return True
	stop
  where
	nochanges = "There have been no changes to the remote's configuration since it was last verified."

remoteVerificationMessage :: RemoteName -> UUID -> RemoteConfig -> Annex (Maybe String)
remoteVerificationMessage remotename u remoteconfig = 
	remoteVerificationMessage' remotename remoteconfig
		<$> Logs.VerifiedRemote.storeVerifyingRemoteLog remoteconfig
		<*> getVerifiedRemoteConfig u

remoteVerificationMessage' :: RemoteName -> RemoteConfig -> VerifyingRemoteConfigId -> RemoteConfig -> Maybe String
remoteVerificationMessage' remotename remoteconfig cid verifiedremoteconfig
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
		, askexpected
		, "If so, run:  " ++ verifysafecommand
		, "If not, run: " ++ verifyunsafecommand
		]
	
	updatemessage = unlines
		[ "There is an updated configuration of the remote:"
		, "\t" ++ displayRemoteConfig visibleremoteconfig
		, ""
		, askexpected
		, "If so, run:  " ++ verifysafecommand
		, "If not, you do not need to do anything; the previously"
		, "verified configuration of the remote will continue to"
		, "be used."
		]
	
	askexpected = "Is this a location that you expected to be using?"

	verifysafecommand = "git-annex verifyremote "
		 ++ remotename ++ " --safe "
		 ++ show (fromVerifyingRemoteConfigId cid)

	verifyunsafecommand = "git-annex verifyremote "
		++ remotename ++ " --unsafe"
	
	visibleremoteconfig = visiblefields remoteconfig
	
	visiblefields = Logs.VerifiedRemote.omitUnverifiableFields
		. flip M.withoutKeys omitfields
	
	-- Fields that do not need to be verified.
	omitfields = S.fromList
		[ nameField
		, sameasNameField
		, sameasUUIDField
		]

performVerifySafe :: VerifyingRemoteConfigId -> Remote -> CommandPerform
performVerifySafe cid r = do
	remoteconfig <- getUnverifiedRemoteConfig (logUUID r)
	m <- Logs.VerifiedRemote.readVerifyingRemoteLog
	case M.lookup cid m of
		Just rc
			| rc == Logs.VerifiedRemote.omitUnverifiableFields remoteconfig -> do
				Logs.VerifiedRemote.configSet (logUUID r) remoteconfig
				Logs.VerifiedRemote.removeVerifyingLog cid
				next $ return True
		_ -> do
			warning "invalid ID"
			next $ return False

performVerifyUnsafe :: Remote -> CommandPerform
performVerifyUnsafe r = do
	m <- fromMaybe mempty <$> Logs.VerifiedRemote.readRemoteLog
	let hasverifiedconfig = M.member (logUUID r) m
	warning $ UnquotedString $ unlines $ 
		warningVerifyUnsafe (Remote.name r) hasverifiedconfig
	next $ return False

warningVerifyUnsafe :: RemoteName -> Bool -> [String]
warningVerifyUnsafe name False =
	[ "So the remote " ++ name ++ " is in a location you did"
	, "not expect to be using. You may be under attack."
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
warningVerifyUnsafe name True =
	[ "Unfortunately, information may have leaked about the content"
	, "of " ++ name ++ "."
	, ""
	, "Either it has already been verified as safe, or it was was"
	, "enabled by a previous version of git-annex that did not verify"
	, "the location of special remotes. So nothing prevented content"
	, "from it being sent to other remotes."
	]

getUnverifiedRemoteConfig :: UUID -> Annex RemoteConfig
getUnverifiedRemoteConfig u = do
	m <- Logs.Remote.readRemoteLog
	return $ fromMaybe mempty (M.lookup u m)

getVerifiedRemoteConfig :: UUID -> Annex RemoteConfig
getVerifiedRemoteConfig u = do
	m <- fromMaybe mempty <$> Logs.VerifiedRemote.readRemoteLog
	return $ fromMaybe mempty (M.lookup u m)

logUUID :: Remote -> UUID
logUUID r = fromMaybe (Remote.uuid r) $
	remoteAnnexConfigUUID (Remote.gitconfig r)

displayRemoteConfig :: RemoteConfig -> String
displayRemoteConfig = intercalate " " . map display . sortBy order . M.toList
  where
	display (f, v) =
		fromProposedAccepted f ++ "=" ++ fromProposedAccepted v
	order (f1, v1) (f2, v2) = 
		conventionalConfigOrder f1 f2 <> compare v1 v2

