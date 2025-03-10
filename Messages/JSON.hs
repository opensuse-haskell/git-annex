{- git-annex command-line JSON output and input
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, GADTs #-}

module Messages.JSON (
	JSONBuilder,
	JSONChunk(..),
	emit,
	emit',
	none,
	start,
	startActionItem,
	end,
	finalize,
	addErrorMessage,
	note,
	info,
	messageid,
	add,
	complete,
	progress,
	DualDisp(..),
	ObjectMap(..),
	JSONActionItem(..),
	AddJSONActionItemField(..),
	module Utility.Aeson,
) where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson.KeyMap as HM
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Data.Maybe
import Data.Monoid
import Prelude

import Types.Command (SeekInput(..))
import Types.ActionItem
import Types.UUID
import Key
import Utility.Metered
import Utility.Percentage
import Utility.Aeson
import Utility.OsPath
import Types.Messages

-- A global lock to avoid concurrent threads emitting json at the same time.
{-# NOINLINE emitLock #-}
emitLock :: MVar ()
emitLock = unsafePerformIO $ newMVar ()

emit :: Object -> IO ()
emit = emit' . encode

emit' :: L.ByteString -> IO ()
emit' b = do
	takeMVar emitLock
	L.hPut stdout b
	putStr "\n"
	putMVar emitLock ()

-- Building up a JSON object can be done by first using start,
-- then add and note and messageid any number of times, and finally
-- complete.
type JSONBuilder = Maybe (Object, Bool) -> Maybe (Object, Bool)

none :: JSONBuilder
none = id

start :: String -> Maybe OsPath -> Maybe Key -> SeekInput -> JSONBuilder
start command file key si _ = case j of
	Object o -> Just (o, False)
	_ -> Nothing
  where
	j = toJSON' $ JSONActionItem
		{ itemCommand = Just command
		, itemKey = key
		, itemFile = file
		, itemUUID = Nothing
		, itemFields = Nothing :: Maybe Bool
		, itemSeekInput = si
		}

startActionItem :: String -> ActionItem -> SeekInput -> JSONBuilder
startActionItem command ai si _ = case j of
	Object o -> Just (o, False)
	_ -> Nothing
  where
	j = toJSON' $ JSONActionItem
		{ itemCommand = Just command
		, itemKey = actionItemKey ai
		, itemFile = actionItemFile ai
		, itemUUID = actionItemUUID ai
		, itemFields = Nothing :: Maybe Bool
		, itemSeekInput = si
		}

end :: Bool -> JSONBuilder
end b (Just (o, _)) = Just (HM.insert "success" (toJSON' b) o, True)
end _ Nothing = Nothing

-- Always include error-messages field, even if empty,
-- to make the json be self-documenting.
finalize :: Object -> Object
finalize o = addErrorMessage [] o

addErrorMessage :: [String] -> Object -> Object
addErrorMessage msg o =
	HM.unionWith combinearray (HM.singleton "error-messages" v) o
  where
	combinearray (Array new) (Array old) = Array (old <> new)
	combinearray new _old = new
	v = Array $ V.fromList $ map (String . packString) msg

note :: String -> JSONBuilder
note _ Nothing = Nothing
note s (Just (o, e)) = Just (HM.unionWith combinelines (HM.singleton "note" (toJSON' s)) o, e)
  where
	combinelines (String new) (String old) =
		String (old <> "\n" <> new)
	combinelines new _old = new

messageid :: MessageId -> JSONBuilder
messageid _ Nothing = Nothing
messageid mid (Just (o, e)) = Just (HM.unionWith replaceold (HM.singleton "message-id" (toJSON' (show mid))) o, e)
  where
	replaceold new _old = new

info :: String -> JSONBuilder
info s _ = case j of
	Object o -> Just (o, True)
	_ -> Nothing
  where
	j = object ["info" .= toJSON' s]

data JSONChunk v where
	AesonObject :: Object -> JSONChunk Object
	JSONChunk :: ToJSON' v => [(String, v)] -> JSONChunk [(String, v)]

add :: JSONChunk v -> JSONBuilder
add v (Just (o, e)) = case j of
	Object o' -> Just (HM.union o' o, e)
	_ -> Nothing
  where
	j = case v of
		AesonObject ao -> Object ao
		JSONChunk l -> object $ map mkPair l
	mkPair (s, d) = (textKey (packString s), toJSON' d)
add _ Nothing = Nothing

complete :: JSONChunk v -> JSONBuilder
complete v _ = add v (Just (HM.empty, True))

-- Show JSON formatted progress, including the current state of the JSON 
-- object for the action being performed.
progress :: Maybe Object -> Maybe TotalSize -> BytesProcessed -> IO ()
progress maction msize bytesprocessed = 
	case j of
		Object o -> emit $ case maction of
			Just action -> HM.insert "action" (Object action) o
			Nothing -> o
		_ -> return ()
  where
	n = fromBytesProcessed bytesprocessed :: Integer
	j = case msize of
		Just (TotalSize size) -> object
			[ "byte-progress" .= n
			, "percent-progress" .= showPercentage 2 (percentage size n)
			, "total-size" .= size
			]
		Nothing -> object
			[ "byte-progress" .= n ]

-- A value that can be displayed either normally, or as JSON.
data DualDisp = DualDisp
	{ dispNormal :: String
	, dispJson :: String
	}

instance ToJSON' DualDisp where
	toJSON' = toJSON' . dispJson

instance Show DualDisp where
	show = dispNormal

-- A Map that is serialized to JSON as an object, with each key being a
-- field of the object. This is different from Aeson's normal 
-- serialization of Map, which uses "[key, value]".
data ObjectMap a = ObjectMap { fromObjectMap :: M.Map String a }

instance ToJSON' a => ToJSON' (ObjectMap a) where
	toJSON' (ObjectMap m) = object $ map go $ M.toList m
	  where
		go (k, v) = (textKey (packString k), toJSON' v)

-- An item that a git-annex command acts on, and displays a JSON object about.
data JSONActionItem a = JSONActionItem
	{ itemCommand :: Maybe String
	, itemKey :: Maybe Key
	, itemFile :: Maybe OsPath
	, itemUUID :: Maybe UUID
	, itemFields :: Maybe a
	, itemSeekInput :: SeekInput
	}
	deriving (Show)

instance ToJSON' a => ToJSON' (JSONActionItem a) where
	toJSON' i = object $ catMaybes
		[ Just $ "command" .= itemCommand i
		, case itemKey i of
			Just k -> Just $ "key" .= toJSON' k
			Nothing -> Nothing
		, case itemFile i of
			Just f -> 
				let f' = (fromOsPath f) :: S.ByteString
				in Just $ "file" .= toJSON' f'
			Nothing -> Nothing
		, case itemFields i of
			Just f -> Just $ "fields" .= toJSON' f
			Nothing -> Nothing
		, case itemUUID i of
			Just u -> Just $ "uuid" .= toJSON' u
			Nothing -> Nothing
		, Just $ "input" .= fromSeekInput (itemSeekInput i)
		]

instance FromJSON a => FromJSON (JSONActionItem a) where
	parseJSON (Object v) = JSONActionItem
		<$> (v .:? "command")
		<*> (maybe (return Nothing) parseJSON =<< (v .:? "key"))
		<*> (fmap stringToOsPath <$> (v .:? "file"))
		<*> (v .:? "uuid")
		<*> (v .:? "fields")
		-- ^ fields is used for metadata, which is currently the
		-- only json that gets parsed
		<*> pure (SeekInput [])
	parseJSON _ = mempty

-- This can be used to populate a field after a JSONActionItem
-- has already been started.
data AddJSONActionItemField a = AddJSONActionItemField String a
	deriving (Show)

instance ToJSON' a => ToJSON' (AddJSONActionItemField a) where
	toJSON' (AddJSONActionItemField f a) = object [ (textKey (packString f), toJSON' a) ]
