{- P2P protocol
 -
 - See doc/design/p2p_protocol.mdwn
 -
 - Copyright 2016-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module P2P.Protocol where

import qualified Utility.SimpleProtocol as Proto
import Types (Annex)
import Types.Key
import Types.UUID
import Types.Transfer
import Types.Remote (Verification(..))
import Utility.Hash (IncrementalVerifier(..))
import Utility.AuthToken
import Utility.Applicative
import Utility.PartialPrelude
import Utility.Metered
import Utility.MonotonicClock
import Utility.OsPath
import qualified Utility.OsString as OS
import Git.FilePath
import Annex.ChangedRefs (ChangedRefs)
import Types.NumCopies

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Catch
import System.Exit (ExitCode(..))
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S
import Data.Char
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Applicative
import Control.DeepSeq
import Prelude

newtype Offset = Offset Integer
	deriving (Show, Eq, NFData, Num, Real, Ord, Enum, Integral)

newtype Len = Len Integer
	deriving (Show)

newtype ProtocolVersion = ProtocolVersion Integer
	deriving (Show, Eq, Ord)

defaultProtocolVersion :: ProtocolVersion
defaultProtocolVersion = ProtocolVersion 0

maxProtocolVersion :: ProtocolVersion
maxProtocolVersion = ProtocolVersion 4

-- In order from newest to oldest.
allProtocolVersions :: [ProtocolVersion]
allProtocolVersions =
	[ ProtocolVersion 4
	, ProtocolVersion 3
	, ProtocolVersion 2
	, ProtocolVersion 1
	, ProtocolVersion 0
	] 

newtype ProtoAssociatedFile = ProtoAssociatedFile AssociatedFile
	deriving (Show)

-- | Service as used by the connect message in gitremote-helpers(1)
data Service = UploadPack | ReceivePack
	deriving (Show)

data Validity = Valid | Invalid
	deriving (Show)
	
newtype Bypass = Bypass (S.Set UUID)
	deriving (Show, Monoid, Semigroup)

-- | Messages in the protocol. The peer that makes the connection
-- always initiates requests, and the other peer makes responses to them.
data Message
	= AUTH UUID AuthToken -- uuid of the peer that is authenticating
	| AUTH_SUCCESS UUID -- uuid of the remote peer
	| AUTH_FAILURE
	| VERSION ProtocolVersion
	| CONNECT Service
	| CONNECTDONE ExitCode
	| NOTIFYCHANGE
	| CHANGED ChangedRefs
	| CHECKPRESENT Key
	| LOCKCONTENT Key
	| UNLOCKCONTENT
	| REMOVE Key
	| REMOVE_BEFORE MonotonicTimestamp Key
	| GETTIMESTAMP
	| GET Offset ProtoAssociatedFile Key
	| PUT ProtoAssociatedFile Key
	| PUT_FROM Offset
	| ALREADY_HAVE
	| ALREADY_HAVE_PLUS [UUID]
	| SUCCESS
	| SUCCESS_PLUS [UUID]
	| FAILURE
	| FAILURE_PLUS [UUID]
	| BYPASS Bypass
	| DATA Len -- followed by bytes of data
	| DATA_PRESENT
	| VALIDITY Validity
	| TIMESTAMP MonotonicTimestamp
	| ERROR String
	deriving (Show)

instance Proto.Sendable Message where
	formatMessage (AUTH uuid authtoken) = ["AUTH", Proto.serialize uuid, Proto.serialize authtoken]
	formatMessage (AUTH_SUCCESS uuid) = ["AUTH-SUCCESS",  Proto.serialize uuid]
	formatMessage AUTH_FAILURE = ["AUTH-FAILURE"]
	formatMessage (VERSION v) = ["VERSION", Proto.serialize v]
	formatMessage (CONNECT service) = ["CONNECT", Proto.serialize service]
	formatMessage (CONNECTDONE exitcode) = ["CONNECTDONE", Proto.serialize exitcode]
	formatMessage NOTIFYCHANGE = ["NOTIFYCHANGE"]
	formatMessage (CHANGED refs) = ["CHANGED", Proto.serialize refs]
	formatMessage (CHECKPRESENT key) = ["CHECKPRESENT", Proto.serialize key]
	formatMessage (LOCKCONTENT key) = ["LOCKCONTENT", Proto.serialize key]
	formatMessage UNLOCKCONTENT = ["UNLOCKCONTENT"]
	formatMessage (REMOVE key) = ["REMOVE", Proto.serialize key]
	formatMessage (REMOVE_BEFORE ts key) = ["REMOVE-BEFORE", Proto.serialize ts, Proto.serialize key]
	formatMessage GETTIMESTAMP = ["GETTIMESTAMP"]
	formatMessage (GET offset af key) = ["GET", Proto.serialize offset, Proto.serialize af, Proto.serialize key]
	formatMessage (PUT af key) = ["PUT", Proto.serialize af, Proto.serialize key]
	formatMessage (PUT_FROM offset) = ["PUT-FROM", Proto.serialize offset]
	formatMessage ALREADY_HAVE = ["ALREADY-HAVE"]
	formatMessage (ALREADY_HAVE_PLUS uuids) = ("ALREADY-HAVE-PLUS":map Proto.serialize uuids)
	formatMessage SUCCESS = ["SUCCESS"]
	formatMessage (SUCCESS_PLUS uuids) = ("SUCCESS-PLUS":map Proto.serialize uuids)
	formatMessage FAILURE = ["FAILURE"]
	formatMessage (FAILURE_PLUS uuids) = ("FAILURE-PLUS":map Proto.serialize uuids)
	formatMessage (BYPASS (Bypass uuids)) = ("BYPASS":map Proto.serialize (S.toList uuids))
	formatMessage (DATA len) = ["DATA", Proto.serialize len]
	formatMessage DATA_PRESENT = ["DATA-PRESENT"]
	formatMessage (VALIDITY Valid) = ["VALID"]
	formatMessage (VALIDITY Invalid) = ["INVALID"]
	formatMessage (TIMESTAMP ts) = ["TIMESTAMP", Proto.serialize ts]
	formatMessage (ERROR err) = ["ERROR", Proto.serialize err]

instance Proto.Receivable Message where
	parseCommand "AUTH" = Proto.parse2 AUTH
	parseCommand "AUTH-SUCCESS" = Proto.parse1 AUTH_SUCCESS
	parseCommand "AUTH-FAILURE" = Proto.parse0 AUTH_FAILURE
	parseCommand "VERSION" = Proto.parse1 VERSION
	parseCommand "CONNECT" = Proto.parse1 CONNECT
	parseCommand "CONNECTDONE" = Proto.parse1 CONNECTDONE
	parseCommand "NOTIFYCHANGE" = Proto.parse0 NOTIFYCHANGE
	parseCommand "CHANGED" = Proto.parse1 CHANGED
	parseCommand "CHECKPRESENT" = Proto.parse1 CHECKPRESENT
	parseCommand "LOCKCONTENT" = Proto.parse1 LOCKCONTENT
	parseCommand "UNLOCKCONTENT" = Proto.parse0 UNLOCKCONTENT
	parseCommand "REMOVE" = Proto.parse1 REMOVE
	parseCommand "REMOVE-BEFORE" = Proto.parse2 REMOVE_BEFORE
	parseCommand "GETTIMESTAMP" = Proto.parse0 GETTIMESTAMP
	parseCommand "GET" = Proto.parse3 GET
	parseCommand "PUT" = Proto.parse2 PUT
	parseCommand "PUT-FROM" = Proto.parse1 PUT_FROM
	parseCommand "ALREADY-HAVE" = Proto.parse0 ALREADY_HAVE
	parseCommand "ALREADY-HAVE-PLUS" = Proto.parseList ALREADY_HAVE_PLUS
	parseCommand "SUCCESS" = Proto.parse0 SUCCESS
	parseCommand "SUCCESS-PLUS" = Proto.parseList SUCCESS_PLUS
	parseCommand "FAILURE" = Proto.parse0 FAILURE
	parseCommand "FAILURE-PLUS" = Proto.parseList FAILURE_PLUS
	parseCommand "BYPASS" = Proto.parseList (BYPASS . Bypass . S.fromList)
	parseCommand "DATA" = Proto.parse1 DATA
	parseCommand "DATA-PRESENT" = Proto.parse0 DATA_PRESENT
	parseCommand "VALID" = Proto.parse0 (VALIDITY Valid)
	parseCommand "INVALID" = Proto.parse0 (VALIDITY Invalid)
	parseCommand "TIMESTAMP" = Proto.parse1 TIMESTAMP
	parseCommand "ERROR" = Proto.parse1 ERROR
	parseCommand _ = Proto.parseFail

instance Proto.Serializable ProtocolVersion where
	serialize (ProtocolVersion n) = show n
	deserialize = ProtocolVersion <$$> readish

instance Proto.Serializable Offset where
	serialize (Offset n) = show n
	deserialize = Offset <$$> readish

instance Proto.Serializable Len where
	serialize (Len n) = show n
	deserialize = Len <$$> readish

instance Proto.Serializable MonotonicTimestamp where
	serialize (MonotonicTimestamp n) = show n
	deserialize = MonotonicTimestamp <$$> readish

instance Proto.Serializable Service where
	serialize UploadPack = "git-upload-pack"
	serialize ReceivePack = "git-receive-pack"
	deserialize "git-upload-pack" = Just UploadPack
	deserialize "git-receive-pack" = Just ReceivePack
	deserialize _ = Nothing

-- | Since ProtoAssociatedFile is not the last thing in a protocol line,
-- its serialization cannot contain any whitespace. This is handled
-- by replacing whitespace with '%' (and '%' with '%%')
--
-- When deserializing an AssociatedFile from a peer, that escaping is
-- reversed. Unfortunately, an input tab will be deescaped to a space
-- though. And it's sanitized, to avoid any control characters that might
-- cause problems when it's displayed to the user.
--
-- These mungings are ok, because a ProtoAssociatedFile is normally
-- only displayed to the user and so does not need to match a file on disk.
-- It may also be used in checking preferred content, which is very
-- unlikely to care about spaces vs tabs or control characters.
instance Proto.Serializable ProtoAssociatedFile where
	serialize (ProtoAssociatedFile (AssociatedFile Nothing)) = ""
	serialize (ProtoAssociatedFile (AssociatedFile (Just af))) = 
		fromOsPath $ toInternalGitPath $
			OS.concat $ map esc $ OS.unpack af
	  where
	  	esc c = case OS.toChar c of
			'%' -> literalOsPath "%%"
			c' | isSpace c' -> literalOsPath "%"
			_ -> OS.singleton c
	
	deserialize s = case fromInternalGitPath $ toOsPath $ deesc [] s of
		f
			| OS.null f -> Just $ ProtoAssociatedFile $
				AssociatedFile Nothing
			| isRelative f -> Just $ ProtoAssociatedFile $ 
				AssociatedFile $ Just f
			| otherwise -> Nothing
	  where
	  	deesc b [] = reverse b
		deesc b ('%':'%':cs) = deesc ('%':b) cs
		deesc b ('%':cs) = deesc ('_':b) cs
		deesc b (c:cs)
			| isControl c = deesc ('_':b) cs
			| otherwise = deesc (c:b) cs

-- | Free monad for the protocol, combining net communication,
-- and local actions.
data ProtoF c = Net (NetF c) | Local (LocalF c)
	deriving (Functor)

type Proto = Free ProtoF

net :: Net a -> Proto a
net = hoistFree Net

local :: Local a -> Proto a
local = hoistFree Local

data NetF c
	= SendMessage Message c
	| ReceiveMessage (Maybe Message -> c)
	| SendBytes Len L.ByteString MeterUpdate c
	-- ^ Sends exactly Len bytes of data. (Any more or less will
	-- confuse the receiver.)
	| ReceiveBytes Len MeterUpdate (L.ByteString -> c)
	-- ^ Streams bytes from peer. Stops once Len are read,
	-- or if connection is lost. This allows resuming
	-- interrupted transfers.
	| CheckAuthToken UUID AuthToken (Bool -> c)
	| RelayService Service c
	-- ^ Runs a service, relays its output to the peer, and data
	-- from the peer to it.
	| Relay RelayHandle RelayHandle (ExitCode -> c)
	-- ^ Reads from the first RelayHandle, and sends the data to a
	-- peer, while at the same time accepting input from the peer
	-- which is sent the the second RelayHandle. Continues until 
	-- the peer sends an ExitCode.
	| SetProtocolVersion ProtocolVersion c
	--- ^ Called when a new protocol version has been negotiated.
	| GetProtocolVersion (ProtocolVersion -> c)
	| GetMonotonicTimestamp (MonotonicTimestamp -> c)
	deriving (Functor)

type Net = Free NetF

newtype RelayHandle = RelayHandle Handle

data LocalF c
	= TmpContentSize Key (Len -> c)
	-- ^ Gets size of the temp file where received content may have
	-- been stored. If not present, returns 0.
	| FileSize OsPath (Len -> c)
	-- ^ Gets size of the content of a file. If not present, returns 0.
	| ContentSize Key (Maybe Len -> c)
	-- ^ Gets size of the content of a key, when the full content is
	-- present.
	| ReadContent Key AssociatedFile (Maybe OsPath) Offset (L.ByteString -> Proto Validity -> Proto (Maybe [UUID])) (Maybe [UUID] -> c)
	-- ^ Reads the content of a key and sends it to the callback.
	-- Must run the callback, or terminate the protocol connection.
	--
	-- May send any amount of data, including L.empty if the content is
	-- not available. The callback must deal with that.
	--
	-- And the content may change while it's being sent.
	-- The callback is passed a validity check that it can run after
	-- sending the content to detect when this happened.
	| StoreContent Key AssociatedFile Offset Len (Proto L.ByteString) (Proto (Maybe Validity)) (Bool -> c)
	-- ^ Stores content to the key's temp file starting at an offset.
	-- Once the whole content of the key has been stored, moves the
	-- temp file into place as the content of the key, and returns True.
	--
	-- Must consume the whole lazy ByteString, or if unable to do
	-- so, terminate the protocol connection.
	--
	-- If the validity check is provided and fails, the content was
	-- changed while it was being sent, so verification of the
	-- received content should be forced.
	--
	-- Note: The ByteString may not contain the entire remaining content
	-- of the key. Only once the temp file size == Len has the whole
	-- content been transferred.
	| StoreContentTo OsPath (Maybe IncrementalVerifier) Offset Len (Proto L.ByteString) (Proto (Maybe Validity)) ((Bool, Verification) -> c)
	-- ^ Like StoreContent, but stores the content to a temp file.
	| SendContentWith (L.ByteString -> Annex (Maybe Validity -> Annex Bool)) (Proto L.ByteString) (Proto (Maybe Validity)) (Bool -> c)
	-- ^ Reads content from the Proto L.ByteString and sends it to the
	-- callback. The callback must consume the whole lazy ByteString,
	-- before it returns a validity checker.
	| SetPresent Key UUID c
	| CheckContentPresent Key (Bool -> c)
	-- ^ Checks if the whole content of the key is locally present.
	| RemoveContent Key (Maybe MonotonicTimestamp) (Bool -> c)
	-- ^ If the content is not present, still succeeds.
	-- May fail if not enough copies to safely drop, etc.
	-- After locking the content for removal, checks if it's later
	-- than the MonotonicTimestamp, and fails.
	| TryLockContent Key (Bool -> Proto ()) c
	-- ^ Try to lock the content of a key,  preventing it
	-- from being deleted, while running the provided protocol
	-- action. If unable to lock the content, or the content is not
	-- present, runs the protocol action with False.
	| WaitRefChange (ChangedRefs -> c)
	-- ^ Waits for one or more git refs to change and returns them.a
	| UpdateMeterTotalSize Meter TotalSize c
	-- ^ Updates the total size of a Meter, for cases where the size is
	-- not known until the data is being received.
	| RunValidityCheck (Annex Validity) (Validity -> c)
	-- ^ Runs a deferred validity check.
	| GetLocalCurrentTime (POSIXTime -> c)
	-- ^ Gets the local time.
	deriving (Functor)

type Local = Free LocalF

-- Generate sendMessage etc functions for all free monad constructors.
$(makeFree ''NetF)
$(makeFree ''LocalF)

auth :: UUID -> AuthToken -> Proto () -> Proto (Maybe UUID)
auth myuuid t a = do
	net $ sendMessage (AUTH myuuid t)
	postAuth a

postAuth :: Proto () -> Proto (Maybe UUID)
postAuth a = do
	r <- net receiveMessage
	case r of
		Just (AUTH_SUCCESS theiruuid) -> do
			a
			return $ Just theiruuid
		Just AUTH_FAILURE -> return Nothing
		_ -> do
			net $ sendMessage (ERROR "auth failed")
			return Nothing

negotiateProtocolVersion :: ProtocolVersion -> Proto ()
negotiateProtocolVersion preferredversion = do
	net $ sendMessage (VERSION preferredversion)
	r <- net receiveMessage
	case r of
		Just (VERSION v) -> net $ setProtocolVersion v
		-- Old server doesn't know about the VERSION command.
		Just (ERROR _) -> net $ setProtocolVersion (ProtocolVersion 0)
		_ -> net $ sendMessage (ERROR "expected VERSION")

sendBypass :: Bypass -> Proto ()
sendBypass bypass@(Bypass s)
	| S.null s = return ()
	| otherwise = do
		ver <- net getProtocolVersion
		if ver >= ProtocolVersion 2
			then net $ sendMessage (BYPASS bypass)
			else return ()

checkPresent :: Key -> Proto (Either String Bool)
checkPresent key = do
	net $ sendMessage (CHECKPRESENT key)
	checkSuccess'

{- Locks content to prevent it from being dropped, while running an action.
 -
 - Note that this only guarantees that the content is locked as long as the
 - connection to the peer remains up. If the connection is unexpectededly
 - dropped, the peer will then unlock the content.
 -}
lockContentWhile 
	:: MonadMask m 
	=> (forall r. r -> Proto r -> m r)
	-> Key
	-> (Bool -> m a)
	-> m a
lockContentWhile runproto key a = bracket setup cleanup a
  where
	setup = runproto False $ do
		net $ sendMessage (LOCKCONTENT key)
		checkSuccess
	cleanup True = runproto () $ net $ sendMessage UNLOCKCONTENT
	cleanup False = return ()

remove :: Maybe SafeDropProof -> Key -> Proto (Either String Bool, Maybe [UUID])
remove proof key = 
	case safeDropProofEndTime =<< proof of
		Nothing -> removeanytime
		Just endtime -> do
			ver <- net getProtocolVersion
			if ver >= ProtocolVersion 3
				then removeBefore endtime key
				-- Peer is too old to support REMOVE-BEFORE
				else removeanytime
  where
	removeanytime = do
		net $ sendMessage (REMOVE key)
		checkSuccessFailurePlus

getTimestamp :: Proto (Either String MonotonicTimestamp)
getTimestamp = do
	net $ sendMessage GETTIMESTAMP
	net receiveMessage >>= \case
		Just (TIMESTAMP ts) -> return (Right ts)
		Just (ERROR err) -> return (Left err)
		_ -> do
			net $ sendMessage (ERROR "expected TIMESTAMP")
			return (Left "protocol error")

removeBefore :: POSIXTime -> Key -> Proto (Either String Bool, Maybe [UUID])
removeBefore endtime key = getTimestamp >>= \case
	Right remotetime -> 
		canRemoveBefore endtime remotetime (local getLocalCurrentTime) >>= \case
			Just remoteendtime -> 
				removeBeforeRemoteEndTime remoteendtime key
			Nothing ->
				return (Right False, Nothing)
	Left err -> return (Left err, Nothing)

{- The endtime is the last local time at which the key can be removed.
 - To tell the remote how long it has to remove the key, get its current
 - timestamp, and add to it the number of seconds from the current local
 - time until the endtime.
 -
 - Order of retrieving timestamps matters. Getting the local time after the
 - remote timestamp means that, if there is some delay in getting the
 - response from the remote, that is reflected in the local time, and so
 - reduces the allowed time.
 -}
canRemoveBefore :: Monad m => POSIXTime -> MonotonicTimestamp -> m POSIXTime -> m (Maybe MonotonicTimestamp)
canRemoveBefore endtime remotetime getlocaltime = do
	localtime <- getlocaltime
	let timeleft = endtime - localtime
	let timeleft' = MonotonicTimestamp (floor timeleft)
	let remoteendtime = remotetime + timeleft'
	return $ if timeleft <= 0
		then Nothing
		else Just remoteendtime

removeBeforeRemoteEndTime :: MonotonicTimestamp -> Key -> Proto (Either String Bool, Maybe [UUID])
removeBeforeRemoteEndTime remoteendtime key = do
	net $ sendMessage $
		REMOVE_BEFORE remoteendtime key
	checkSuccessFailurePlus	

get :: OsPath -> Key -> Maybe IncrementalVerifier -> AssociatedFile -> Meter -> MeterUpdate -> Proto (Bool, Verification)
get dest key iv af m p = 
	receiveContent (Just m) p sizer storer noothermessages $ \offset ->
		GET offset (ProtoAssociatedFile af) key
  where
	sizer = fileSize dest
	storer = storeContentTo dest iv
	noothermessages = const Nothing

put :: Key -> AssociatedFile -> MeterUpdate -> Proto (Maybe [UUID])
put key af p = put' key af $ \offset ->
	sendContent key af Nothing offset p

put' :: Key -> AssociatedFile -> (Offset -> Proto (Maybe [UUID])) -> Proto (Maybe [UUID])
put' key af sender = do
	net $ sendMessage (PUT (ProtoAssociatedFile af) key)
	r <- net receiveMessage
	case r of
		Just (PUT_FROM offset) -> sender offset
		Just ALREADY_HAVE -> return (Just [])
		Just (ALREADY_HAVE_PLUS uuids) -> return (Just uuids)
		_ -> do
			net $ sendMessage (ERROR "expected PUT_FROM or ALREADY_HAVE")
			return Nothing

-- The protocol does not have a way to get the PUT offset
-- without sending DATA, so send an empty bytestring and indicate
-- it is not valid.
getPutOffset :: Key -> AssociatedFile -> Proto (Either [UUID] Offset)
getPutOffset key af = do
	net $ sendMessage (PUT (ProtoAssociatedFile af) key)
	r <- net receiveMessage
	case r of
		Just (PUT_FROM offset) -> do
			void $ sendContent' nullMeterUpdate (Len 0) L.empty $
				return Invalid
			return (Right offset)
		Just ALREADY_HAVE -> return (Left [])
		Just (ALREADY_HAVE_PLUS uuids) -> return (Left uuids)
		_ -> do
			net $ sendMessage (ERROR "expected PUT_FROM or ALREADY_HAVE")
			return (Left [])

data ServerHandler a
	= ServerGot a
	| ServerContinue
	| ServerUnexpected

-- Server loop, getting messages from the client and handling them
serverLoop :: (Message -> Proto (ServerHandler a)) -> Proto (Maybe a)
serverLoop a = serveOneMessage a serverLoop

-- Get one message from the client and handle it.
serveOneMessage
	:: (Message -> Proto (ServerHandler a))
	-> ((Message -> Proto (ServerHandler a)) -> Proto (Maybe a)) 
	-> Proto (Maybe a)
serveOneMessage a cont = do
	mcmd <- net receiveMessage
	case mcmd of
		-- When the client sends ERROR to the server, the server
		-- gives up, since it's not clear what state the client
		-- is in, and so not possible to recover.
		Just (ERROR _) -> return Nothing
		-- When the client sends an unparsable message, the server
		-- responds with an error message, and continues. This allows
		-- expanding the protocol with new messages.
		Nothing -> do
			net $ sendMessage (ERROR "unknown command")
			cont a
		Just cmd -> do
			v <- a cmd
			case v of
				ServerGot r -> return (Just r)
				ServerContinue -> cont a
				-- If the client sends an unexpected message,
				-- the server will respond with ERROR, and
				-- always continues processing messages.
				--
				-- Since the protocol is not versioned, this
				-- is necessary to handle protocol changes
				-- robustly, since the client can detect when
				-- it's talking to a server that does not
				-- support some new feature, and fall back.
				ServerUnexpected -> do
					net $ sendMessage (ERROR "unexpected command")
					cont a

-- | Serve the protocol, with an unauthenticated peer. Once the peer
-- successfully authenticates, returns their UUID.
serveAuth :: UUID -> Proto (Maybe UUID)
serveAuth myuuid = serverLoop handler
  where
	handler (AUTH theiruuid authtoken) = do
		ok <- net $ checkAuthToken theiruuid authtoken
		if ok
			then do
				net $ sendMessage (AUTH_SUCCESS myuuid)
				return (ServerGot theiruuid)
			else do
				net $ sendMessage AUTH_FAILURE
				return ServerContinue
	handler _ = return ServerUnexpected

data ServerMode
	= ServeReadOnly
	-- ^ Allow reading, but not writing.
	| ServeAppendOnly
	-- ^ Allow reading, and storing new objects, but not deleting objects.
	| ServeReadWrite
	-- ^ Full read and write access.
	deriving (Show, Eq, Ord)

-- | Serve the protocol, with a peer that has authenticated.
serveAuthed :: ServerMode -> UUID -> Proto ()
serveAuthed servermode myuuid = void $ serverLoop $
	serverHandler servermode myuuid

-- | Serve a single command in the protocol, the same as serveAuthed,
-- but without looping to handle the next command.
serveOneCommandAuthed :: ServerMode -> UUID -> Proto ()
serveOneCommandAuthed servermode myuuid = fromMaybe () <$>
	serveOneMessage (serverHandler servermode myuuid) 
		(const $ pure Nothing)

serverHandler :: ServerMode -> UUID -> Message -> Proto (ServerHandler ())
serverHandler servermode myuuid = handler
  where
	handler (VERSION theirversion) = do
		let v = min theirversion maxProtocolVersion
		net $ setProtocolVersion v
		net $ sendMessage (VERSION v)
		return ServerContinue
	handler (LOCKCONTENT key) = do
		local $ tryLockContent key $ \locked -> do
			sendSuccess locked
			when locked $ do
				r' <- net receiveMessage
				case r' of
					Just UNLOCKCONTENT -> return ()
					_ -> net $ sendMessage (ERROR "expected UNLOCKCONTENT")
		return ServerContinue
	handler (CHECKPRESENT key) = do
		sendSuccess =<< local (checkContentPresent key)
		return ServerContinue
	handler (REMOVE key) =
		handleremove key Nothing
	handler (REMOVE_BEFORE ts key) =
		handleremove key (Just ts)
	handler GETTIMESTAMP = do
		ts <- net getMonotonicTimestamp
		net $ sendMessage $ TIMESTAMP ts
		return ServerContinue
	handler (PUT (ProtoAssociatedFile af) key) =
		checkPUTServerMode servermode $ \case
			Nothing -> handleput af key
			Just notallowed -> do
				notallowed
				return ServerContinue
	handler (GET offset (ProtoAssociatedFile af) key) = do
		void $ sendContent key af Nothing offset nullMeterUpdate
		-- setPresent not called because the peer may have
		-- requested the data but not permanently stored it.
		return ServerContinue
	handler (CONNECT service) = do
		-- After connecting to git, there may be unconsumed data
		-- from the git processes hanging around (even if they
		-- exited successfully), so stop serving this connection.
		let endit = return $ ServerGot ()
		checkCONNECTServerMode service servermode $ \case
			Nothing -> do
				net $ relayService service
				endit
			Just notallowed -> do
				notallowed
				endit
	handler NOTIFYCHANGE = do
		refs <- local waitRefChange
		net $ sendMessage (CHANGED refs)
		return ServerContinue
	handler (BYPASS _) = return ServerContinue
	handler _ = return ServerUnexpected

	handleput af key = do
		have <- local $ checkContentPresent key
		if have
			then net $ sendMessage ALREADY_HAVE
			else do
				let sizer = tmpContentSize key
				let storer = storeContent key af
				ver <- net getProtocolVersion
				let handleothermessages
					| ver >= ProtocolVersion 4 = \case
						DATA_PRESENT -> Just $
							checkContentPresent key
						_ -> Nothing
					| otherwise = const Nothing
				v <- receiveContent Nothing nullMeterUpdate 
					sizer storer handleothermessages 
					PUT_FROM
				when (observeBool v) $
					local $ setPresent key myuuid
		return ServerContinue
		
	handleremove key mts =
		checkREMOVEServerMode servermode $ \case
			Nothing -> do
				sendSuccess =<< local (removeContent key mts)
				return ServerContinue			
			Just notallowed -> do
				notallowed
				return ServerContinue

sendReadOnlyError :: Proto ()
sendReadOnlyError = net $ sendMessage $
	ERROR "this repository is read-only; write access denied"

sendAppendOnlyError :: Proto ()
sendAppendOnlyError = net $ sendMessage $
	ERROR "this repository is append-only; removal denied"
			
checkPUTServerMode :: Monad m => ServerMode -> (Maybe (Proto ()) -> m a) -> m a
checkPUTServerMode servermode a =
	case servermode of
		ServeReadWrite -> a Nothing
		ServeAppendOnly -> a Nothing
		ServeReadOnly -> a (Just sendReadOnlyError)

checkREMOVEServerMode :: Monad m => ServerMode -> (Maybe (Proto ()) -> m a) -> m a
checkREMOVEServerMode servermode a =
	case servermode of
		ServeReadWrite -> a Nothing
		ServeAppendOnly -> a (Just sendAppendOnlyError)
		ServeReadOnly -> a (Just sendReadOnlyError)

checkCONNECTServerMode :: Monad m => Service -> ServerMode -> (Maybe (Proto ()) -> m a) -> m a
checkCONNECTServerMode service servermode a =
	case (servermode, service) of
		(ServeReadWrite, _) -> a Nothing
		(ServeAppendOnly, UploadPack) -> a Nothing
		-- git protocol could be used to overwrite
		-- refs or something, so don't allow
		(ServeAppendOnly, ReceivePack) -> a (Just sendReadOnlyError)
		(ServeReadOnly, UploadPack) -> a Nothing
		(ServeReadOnly, ReceivePack) -> a (Just sendReadOnlyError)

sendContent :: Key -> AssociatedFile -> Maybe OsPath -> Offset -> MeterUpdate -> Proto (Maybe [UUID])
sendContent key af o offset@(Offset n) p = go =<< local (contentSize key)
  where
	go (Just (Len totallen)) = do
		let len = totallen - n
		if len <= 0
			then sender (Len 0) L.empty (return Valid)
			else local $ readContent key af o offset $
				sender (Len len)
	-- Content not available to send. Indicate this by sending
	-- empty data and indicate it's invalid.
 	go Nothing = sender (Len 0) L.empty (return Invalid)

	sender = sendContent' p'
	
	p' = offsetMeterUpdate p (toBytesProcessed n)

sendContent' :: MeterUpdate -> Len -> L.ByteString -> Proto Validity -> Proto (Maybe [UUID])
sendContent' p len content validitycheck = do
	net $ sendMessage (DATA len)
	net $ sendBytes len content p
	ver <- net getProtocolVersion
	when (ver >= ProtocolVersion 1) $
		net . sendMessage . VALIDITY =<< validitycheck
	checkSuccessPlus

receiveContent
	:: Observable t
	=> Maybe Meter
	-> MeterUpdate
	-> Local Len
	-> (Offset -> Len -> Proto L.ByteString -> Proto (Maybe Validity) -> Local t)
	-> (Message -> Maybe (Local t))
	-> (Offset -> Message)
	-> Proto t
receiveContent mm p sizer storer handleothermessages mkmsg = do
	Len n <- local sizer
	let p' = offsetMeterUpdate p (toBytesProcessed n)
	let offset = Offset n
	net $ sendMessage (mkmsg offset)
	r <- net receiveMessage
	case r of
		Just (DATA len@(Len l)) -> do
			local $ case mm of
				Nothing -> return ()
				Just m -> updateMeterTotalSize m (TotalSize (n+l))
			ver <- net getProtocolVersion
			let validitycheck = if ver >= ProtocolVersion 1
				then net receiveMessage >>= \case
					Just (VALIDITY v) -> return (Just v)
					_ -> do
						net $ sendMessage (ERROR "expected VALID or INVALID")
						return Nothing
				else return Nothing
			sendresultof $ storer offset len
				(net (receiveBytes len p'))
				validitycheck
		Just (ERROR _err) ->
			return observeFailure
		Just msg -> 
			maybe unsupportedmessage sendresultof
				(handleothermessages msg)
		Nothing -> unsupportedmessage
  where
	unsupportedmessage = do
		net $ sendMessage (ERROR "expected DATA")
		return observeFailure
	sendresultof a = do
		v <- local a
		sendSuccess (observeBool v)
		return v

checkSuccess :: Proto Bool
checkSuccess = either (const False) id <$> checkSuccess'

checkSuccess' :: Proto (Either String Bool)
checkSuccess' = do
	ack <- net receiveMessage
	case ack of
		Just SUCCESS -> return (Right True)
		Just FAILURE -> return (Right False)
		Just (ERROR err) -> return (Left err)
		_ -> do
			net $ sendMessage (ERROR "expected SUCCESS or FAILURE")
			return (Right False)

checkSuccessPlus :: Proto (Maybe [UUID])
checkSuccessPlus =
	checkSuccessFailurePlus >>= return . \case
		(Right True, v) -> v
		(Right False, _) -> Nothing
		(Left _, _) -> Nothing

checkSuccessFailurePlus :: Proto (Either String Bool, Maybe [UUID])
checkSuccessFailurePlus = do
	ver <- net getProtocolVersion
	if ver >= ProtocolVersion 2
		then do
			ack <- net receiveMessage
			case ack of
				Just SUCCESS -> return (Right True, Just [])
				Just (SUCCESS_PLUS l) -> return (Right True, Just l)
				Just FAILURE -> return (Right False, Nothing)
				Just (FAILURE_PLUS l) -> return (Right False, Just l)
				Just (ERROR err) -> return (Left err, Nothing)
				_ -> do
					net $ sendMessage (ERROR "expected SUCCESS or SUCCESS-PLUS or FAILURE or FAILURE-PLUS")
					return (Right False, Nothing)
		else do
			ok <- checkSuccess
			if ok
				then return (Right True, Just [])
				else return (Right False, Nothing)

sendSuccess :: Bool -> Proto ()
sendSuccess True = net $ sendMessage SUCCESS
sendSuccess False = net $ sendMessage FAILURE

notifyChange :: Proto (Maybe ChangedRefs)
notifyChange = do
	net $ sendMessage NOTIFYCHANGE
	ack <- net receiveMessage
	case ack of
		Just (CHANGED rs) -> return (Just rs)
		_ -> do
			net $ sendMessage (ERROR "expected CHANGED")
			return Nothing

connect :: Service -> Handle -> Handle -> Proto ExitCode
connect service hin hout = do
	net $ sendMessage (CONNECT service)
	net $ relay (RelayHandle hin) (RelayHandle hout)

data RelayData
	= RelayToPeer L.ByteString
	| RelayFromPeer L.ByteString
	| RelayDone ExitCode
	deriving (Show)

relayFromPeer :: Net RelayData
relayFromPeer = do
	r <- receiveMessage
	case r of
		Just (CONNECTDONE exitcode) -> return $ RelayDone exitcode
		Just (DATA len) -> RelayFromPeer <$> receiveBytes len nullMeterUpdate
		_ -> do
			sendMessage $ ERROR "expected DATA or CONNECTDONE"
			return $ RelayDone $ ExitFailure 1

relayToPeer :: RelayData -> Net ()
relayToPeer (RelayDone exitcode) = sendMessage (CONNECTDONE exitcode)
relayToPeer (RelayToPeer b) = do
	let len = Len $ fromIntegral $ L.length b
	sendMessage (DATA len)
	sendBytes len b nullMeterUpdate
relayToPeer (RelayFromPeer _) = return ()

