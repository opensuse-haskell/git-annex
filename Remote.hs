{- git-annex remotes
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote (
	Remote,
	uuid,
	name,
	action,
	verifiedAction,
	storeKey,
	retrieveKeyFile,
	retrieveKeyFileCheap,
	retrievalSecurityPolicy,
	removeKey,
	hasKey,
	hasKeyCheap,
	whereisKey,
	remoteFsck,

	remoteTypes,
	remoteList,
	remoteList',
	remoteMap,
	remoteMap',
	uuidDescriptions,
	addName,
	byName,
	byName',
	byNameOrGroup,
	byNameOnly,
	byNameWithUUID,
	byUUID,
	byCost,
	prettyPrintUUIDs,
	prettyPrintUUIDsDescs,
	prettyPrintUUIDsWith,
	prettyPrintUUIDsWith',
	prettyListUUIDs,
	prettyUUID,
	remoteFromUUID,
	remotesWithUUID,
	remotesWithoutUUID,
	keyLocations,
	IncludeIgnored(..),
	keyPossibilities,
	remoteLocations,
	nameToUUID,
	nameToUUID',
	nameToUUID'',
	showTriedRemotes,
	listRemoteNames,
	showLocations,
	forceTrust,
	logStatus,
	checkAvailable,
	claimingUrl,
	claimingUrl',
	isExportSupported,
	gitSyncableRemote,
	gitSyncableRemoteType,
) where

import Data.Ord
import Data.String
import qualified Data.Map as M
import qualified Data.Vector as V

import Annex.Common
import Types.Remote
import qualified Annex
import Annex.UUID
import Annex.Action
import Logs.UUID
import Logs.Trust
import Logs.Location hiding (logStatus)
import Logs.Remote
import Logs.Web
import Remote.List
import Remote.List.Util
import Config
import Config.DynamicConfig
import Git.Types (RemoteName, ConfigKey(..), fromConfigValue)
import Utility.Aeson

{- Map from UUIDs of Remotes to a calculated value. -}
remoteMap :: (Remote -> v) -> Annex (M.Map UUID v)
remoteMap mkv = remoteMap' mkv (pure . mkk)
  where
	mkk r = case uuid r of
		NoUUID -> Nothing
		u -> Just u

remoteMap' :: Ord k => (Remote -> v) -> (Remote -> Annex (Maybe k)) -> Annex (M.Map k v)
remoteMap' mkv mkk = M.fromList . catMaybes <$> (mapM mk =<< remoteList)
  where
	mk r = mkk r >>= return . \case
		Nothing -> Nothing
		Just k -> Just (k, mkv r)

{- Map of UUIDs of repositories and their descriptions.
 - The names of Remotes are added to supplement any description that has
 - been set for a repository.  -}
uuidDescriptions :: Annex UUIDDescMap
uuidDescriptions = M.unionWith addName
	<$> uuidDescMap
	<*> remoteMap (UUIDDesc . encodeBS . name)

{- Add a remote name to its description. -}
addName :: (IsString t, Monoid t, Eq t) => t -> t -> t
addName desc n
	| desc == n || desc == mempty = "[" <> n <> "]"
	| otherwise = desc <> " [" <> n <> "]"

byUUID :: UUID -> Annex (Maybe Remote)
byUUID u = headMaybe . filter matching <$> remoteList
  where
	matching r = uuid r == u

{- When a name is specified, looks up the remote matching that name.
 - (Or it can be a UUID.)
 -
 - Throws an error if a name is specified and no matching remote can be
 - found.
 -}
byName :: Maybe RemoteName -> Annex (Maybe Remote)
byName Nothing = return Nothing
byName (Just n) = either giveup Just <$> byName' n

{- Like byName, but the remote must have a configured UUID. -}
byNameWithUUID :: Maybe RemoteName -> Annex (Maybe Remote)
byNameWithUUID = checkuuid <=< byName
  where
	checkuuid Nothing = return Nothing
	checkuuid (Just r)
		| uuid r == NoUUID = do
			repo <- getRepo r
			ifM (liftIO $ getDynamicConfig $ remoteAnnexIgnore (gitconfig r))
				( giveup $ noRemoteUUIDMsg r ++
					" (" ++ show (remoteAnnexConfig repo "ignore") ++
					" is set)"
				, giveup $ noRemoteUUIDMsg r
				)
		| otherwise = return $ Just r

byName' :: RemoteName -> Annex (Either String Remote)
byName' n = byName'' n <$> remoteList

byName'' :: RemoteName -> [Remote] -> Either String Remote
byName'' "" _ = Left "no repository name specified"
byName'' n remotelist = go $ filter matching remotelist
  where
	go [] = Left $ "there is no available git remote named \"" ++ n ++ "\""
	go (match:_) = Right match
	matching r = n == name r || toUUID n == uuid r

{- Finds the remote or remote group matching the name. -}
byNameOrGroup :: RemoteName -> Annex [Remote]
byNameOrGroup n = go =<< getConfigMaybe (ConfigKey ("remotes." <> encodeBS n))
  where
	go (Just l) = catMaybes
		<$> mapM (byName . Just) (splitc ' ' (fromConfigValue l))
	go Nothing = maybeToList
		<$> byName (Just n)

{- Only matches remote name, not UUID -}
byNameOnly :: RemoteName -> Annex (Maybe Remote)
byNameOnly n = headMaybe . filter matching <$> remoteList
  where
	matching r = n == name r

noRemoteUUIDMsg :: Remote -> String
noRemoteUUIDMsg r = "cannot determine uuid for " ++ name r ++ " (perhaps you need to run \"git annex sync\"?)"

{- Looks up a remote by name (or by UUID, or even by description),
 - and returns its UUID. Finds even repositories that are not
 - configured in .git/config. -}
nameToUUID :: RemoteName -> Annex UUID
nameToUUID n = nameToUUID' n >>= \case
	([u], _) -> return u
	(_, msg) -> giveup msg

nameToUUID' :: RemoteName -> Annex ([UUID], String)
nameToUUID' n = do
	f <- nameToUUID''
	return (f n)

nameToUUID'' :: Annex (RemoteName -> ([UUID], String))
nameToUUID'' = do
	l <- remoteList
	u <- getUUID
	m <- uuidDescMap
	return $ \n -> nameToUUID''' n l u m

nameToUUID''' :: RemoteName -> [Remote] -> UUID -> UUIDDescMap -> ([UUID], String)
nameToUUID''' n remotelist hereu m
	| n == "." = currentrepo
	| n == "here" = currentrepo
	| otherwise = go (byName'' n remotelist)
  where
	currentrepo = mkone hereu

	go (Right r) = case uuid r of
		NoUUID -> ([], noRemoteUUIDMsg r)
		u -> mkone u
	go (Left e) =
		let descn = UUIDDesc (encodeBS n)
		in case M.keys (M.filter (== descn) m) of
			[] -> 
				let u = toUUID n
				in case M.keys (M.filterWithKey (\k _ -> k == u) m) of
					[] -> ([], e)
					_ -> ([u], e)
			us -> (us, "found multiple repositories with that description (use the uuid instead to disambiguate)")

	mkone u = ([u], "found a remote")

{- Pretty-prints a list of UUIDs of remotes, with their descriptions,
 - for human display.
 -
 - When JSON is enabled, also outputs a machine-readable description
 - of the UUIDs. -}
prettyPrintUUIDs :: String -> [UUID] -> Annex String
prettyPrintUUIDs header uuids = do
	descm <- uuidDescriptions
	prettyPrintUUIDsDescs header descm uuids

prettyPrintUUIDsDescs :: String -> UUIDDescMap -> [UUID] -> Annex String
prettyPrintUUIDsDescs header descm uuids =
	prettyPrintUUIDsWith Nothing header descm
		(const Nothing)
		(zip uuids (repeat (Nothing :: Maybe String)))

{- An optional field can be included in the list of UUIDs. -}
prettyPrintUUIDsWith
	:: ToJSON' v
	=> Maybe String 
	-> String 
	-> UUIDDescMap
	-> (v -> Maybe String)
	-> [(UUID, Maybe v)] 
	-> Annex String
prettyPrintUUIDsWith = prettyPrintUUIDsWith' True

prettyPrintUUIDsWith'
	:: ToJSON' v
	=> Bool
	-> Maybe String 
	-> String 
	-> UUIDDescMap
	-> (v -> Maybe String)
	-> [(UUID, Maybe v)] 
	-> Annex String
prettyPrintUUIDsWith' indented optfield header descm showval uuidvals = do
	hereu <- getUUID
	maybeShowJSON $ JSONChunk [(header, V.fromList $ map (jsonify hereu) uuidvals)]
	return $ concatMap 
		(\u -> tabindent ++ prettify hereu u ++ "\n")
		uuidvals
  where
	tabindent = if indented then "\t" else ""
	finddescription u = fromUUIDDesc $ M.findWithDefault mempty u descm
	prettify hereu (u, optval)
		| not (null d) = addoptval $ fromUUID u ++ " -- " ++ d
		| otherwise = addoptval $ fromUUID u
	  where
		ishere = hereu == u
		n = finddescription u
		d
			| null n && ishere = "here"
			| ishere = addName n "here"
			| otherwise = n
		addoptval s = case showval =<< optval of
			Nothing -> s
			Just val -> val ++ s
	jsonify hereu (u, optval) = object $ catMaybes
		[ Just ("uuid", toJSON' (fromUUID u :: String))
		, Just ("description", toJSON' $ finddescription u)
		, Just ("here", toJSON' $ hereu == u)
		, case (optfield, optval) of
			(Just field, Just val) -> Just
				(textKey (packString field), toJSON' val)
			_ -> Nothing
		]

{- List of remote names and/or descriptions, for human display.  -}
prettyListUUIDs :: [UUID] -> Annex [String]
prettyListUUIDs uuids = do
	hereu <- getUUID
	m <- uuidDescriptions
	return $ map (fromUUIDDesc . prettify m hereu) uuids
  where
	finddescription m u = M.findWithDefault mempty u m
	prettify m hereu u
		| u == hereu = addName n "here"
		| otherwise = n
	  where
		n = finddescription m u

{- Nice display of a remote's name and/or description. -}
prettyUUID :: UUID -> Annex String
prettyUUID u = concat <$> prettyListUUIDs [u]

{- Gets the remote associated with a UUID. -}
remoteFromUUID :: UUID -> Annex (Maybe Remote)
remoteFromUUID u = ifM ((==) u <$> getUUID)
	( return Nothing
	, maybe tryharder (return . Just) =<< findinmap
	)
  where
	findinmap = M.lookup u <$> remoteMap id
	{- Re-read remote list in case a new remote has popped up. -}
	tryharder = do
		remotesChanged
		findinmap

{- List of repository UUIDs that the location log indicates may have a key.
 - Dead repositories are excluded. -}
keyLocations :: Key -> Annex [UUID]
keyLocations key = trustExclude DeadTrusted =<< loggedLocations key

{- Cost ordered lists of remotes that the location log indicates
 - may have a key.
 -
 - Also includes remotes with remoteAnnexSpeculatePresent set.
 -}
keyPossibilities :: IncludeIgnored -> Key -> Annex [Remote]
keyPossibilities ii key = do
	locations <- keyLocations key
	keyPossibilities' ii key locations =<< remoteList

{- Given a list of locations of a key, and a list of all
 - trusted repositories, generates a cost-ordered list of
 - remotes that contain the key, and a list of trusted locations of the key.
 -}
remoteLocations :: IncludeIgnored -> [UUID] -> [UUID] -> Annex ([Remote], [UUID])
remoteLocations ii locations trusted = 
	remoteLocations' ii locations trusted =<< remoteList

{- Displays known locations of a key and helps the user take action
 - to make them accessible. -}
showLocations :: Bool -> Key -> (UUID -> Annex Bool) -> String -> Annex ()
showLocations separateuntrusted key checkexclude nolocmsg = do
	u <- getUUID
	remotes <- remoteList
	uuids <- keyLocations key
	exclude <- filterM checkexclude uuids
	untrusteduuids <- if separateuntrusted
		then trustGet UnTrusted
		else pure []
	let uuidswanted = filteruuids uuids (u:exclude++untrusteduuids)
	let uuidsskipped = filteruuids uuids (u:exclude++uuidswanted)
	let remoteuuids = map uuid remotes
	let isremoteuuid x = elem x remoteuuids
	let (remotesmakeavailable, uuidsothers) = 
		partition isremoteuuid uuidswanted
	isspecialremote <- flip M.member <$> remoteConfigMap
	let (enablespecialremotes, addgitremotes) = 
		partition isspecialremote uuidsothers
	
	-- Add "wanted" field to the JSON. While it's since been split
	-- up more, this avoids breaking any JSON parsers that expect it.
	ifM jsonOutputEnabled
		( void $ prettyPrintUUIDs "wanted" uuidswanted
		, do
			ppremotesmakeavailable <- pp "remotes" remotesmakeavailable
				"Try making some of these remotes available"
			ppenablespecialremotes <- pp "enableremote" enablespecialremotes
				"Maybe enable some of these special remotes (git annex enableremote ...)"
			ppaddgitremotes <- pp "repos" addgitremotes
				"Maybe add some of these git remotes (git remote add ...)"
			ppuuidsskipped <- pp "skipped" uuidsskipped
				"Also these untrusted repositories may contain the file"
			showLongNote $ UnquotedString $ case ppremotesmakeavailable ++ ppenablespecialremotes ++ ppaddgitremotes ++ ppuuidsskipped of
				[] -> nolocmsg
				s -> s
		)
	ignored <- filterM (liftIO . getDynamicConfig . remoteAnnexIgnore . gitconfig) remotes
	unless (null ignored) $
		showLongNote $ UnquotedString $ "(Note that these git remotes have annex-ignore set: " ++ unwords (map name ignored) ++ ")"
  where
	filteruuids l x = filter (`notElem` x) l

	pp jh l h = addheader h <$> prettyPrintUUIDs jh l

	addheader _ [] = []
	addheader h l = h ++ ":\n" ++ l

showTriedRemotes :: [Remote] -> Annex ()
showTriedRemotes [] = noop
showTriedRemotes remotes =
	showLongNote $ UnquotedString $ "Unable to access these remotes: "
		++ listRemoteNames remotes

listRemoteNames :: [Remote] -> String
listRemoteNames remotes = intercalate ", " (map name remotes)

forceTrust :: TrustLevel -> String -> Annex ()
forceTrust level remotename = do
	u <- nameToUUID remotename
	if level >= Trusted
		then toplevelWarning False "Ignoring request to trust repository, because that can lead to data loss."
		else Annex.changeState $ \s ->
			s { Annex.forcetrust = M.insert u level (Annex.forcetrust s) }

{- Used to log a change in a remote's having a key. The change is logged
 - in the local repo, not on the remote. The process of transferring the
 - key to the remote, or removing the key from it *may* log the change
 - on the remote, but this cannot always be relied on. -}
logStatus :: LiveUpdate -> Remote -> Key -> LogStatus -> Annex ()
logStatus lu remote key = logChange lu key (uuid remote)

{- Orders remotes by cost, with ones with the lowest cost grouped together. -}
byCost :: [Remote] -> [[Remote]]
byCost = map snd . sortBy (comparing fst) . M.toList . costmap
  where
	costmap = M.fromListWith (++) . map costpair
	costpair r = (cost r, [r])

checkAvailable :: Bool -> Remote -> Annex Bool
checkAvailable assumenetworkavailable r = tryNonAsync (availability r) >>= \case
	Left _e -> return assumenetworkavailable
	Right LocallyAvailable -> return True
	Right GloballyAvailable -> return assumenetworkavailable
	Right Unavailable -> return False

hasKey :: Remote -> Key -> Annex (Either String Bool)
hasKey r k = either (Left  . show) Right <$> tryNonAsync (checkPresent r k)

hasKeyCheap :: Remote -> Bool
hasKeyCheap = checkPresentCheap

{- The web special remote claims urls by default. -}
claimingUrl :: URLString -> Annex Remote
claimingUrl = claimingUrl' (const True)

{- The web special remote still claims urls if there is no
 - other remote that does, even when the remotefilter does
 - not include it. -}
claimingUrl' :: (Remote -> Bool) -> URLString -> Annex Remote
claimingUrl' remotefilter url = do
	rs <- remoteList
	let web = fromMaybe (error "internal") $ headMaybe $
		filter (\r -> uuid r == webUUID) rs
	fromMaybe web <$> firstM checkclaim (filter remotefilter rs)
  where
	checkclaim = maybe (pure False) (`id` url) . claimUrl

{- Is this a remote of a type that git pull and push work with?
 - That includes special remotes with an annex:: url configured.
 - It does not include proxied remotes. -}
gitSyncableRemote :: Remote -> Bool
gitSyncableRemote r
	| gitSyncableRemoteType (remotetype r) 
		&& isJust (remoteUrl (gitconfig r)) =
			not (isJust (remoteAnnexProxiedBy (gitconfig r)))
	| otherwise = case remoteUrl (gitconfig r) of
		Just u | "annex::" `isPrefixOf` u -> True
		_ -> False
