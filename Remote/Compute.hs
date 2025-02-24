{- Compute remote.
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Compute (
	remote,
	ComputeState(..),
	setComputeState,
	getComputeStates,
	getComputeProgram,
	runComputeProgram,
) where

import Annex.Common
import Types.Remote
import Types.ProposedAccepted
import Types.MetaData
import Types.Creds
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.SpecialRemote.Config
import Annex.UUID
import Annex.Content
import Annex.Tmp
import Logs.MetaData
import Utility.Metered
import Utility.TimeStamp
import Utility.Env
import qualified Git
import qualified Utility.SimpleProtocol as Proto

import Network.HTTP.Types.URI
import Data.Time.Clock
import Text.Read
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

remote :: RemoteType
remote = RemoteType
	{ typename = "compute"
	, enumerate = const $ findSpecialRemotes "compute"
	, generate = gen
	, configParser = computeConfigParser
	, setup = setupInstance
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = case getComputeProgram rc of
	Left _err -> return Nothing
	Right program -> do
		c <- parsedRemoteConfig remote rc
		cst <- remoteCost gc c veryExpensiveRemoteCost
		return $ Just $ mk program c cst
  where
	mk program c cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyUnsupported
 		, retrieveKeyFile = computeKey rs program
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey rs
		, lockContent = Nothing
		, checkPresent = checkKey rs
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, getRepo = return r
		, readonly = True
		, appendonly = False
		, untrustworthy = False
		, availability = pure LocallyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

setupInstance :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
setupInstance _ mu _ c _ = do
	ComputeProgram program <- either giveup return (getComputeProgram c)
	unlessM (liftIO $ inSearchPath program) $
		giveup $ "Cannot find " ++ program ++ " in PATH"
	u <- maybe (liftIO genUUID) return mu
	gitConfigSpecialRemote u c [("compute", "true")]
	return (c, u)

computeConfigParser :: RemoteConfig -> Annex RemoteConfigParser
computeConfigParser _ = return $ RemoteConfigParser
	{ remoteConfigFieldParsers = 
		[ optionalStringParser programField
			(FieldDesc $ "compute program (must start with \"" ++ safetyPrefix ++ "\")")
		]
	-- Pass through all other params, which git-annex addcomputed adds
	-- to the input params.
	, remoteConfigRestPassthrough = Just
		( const True
		, [("*", FieldDesc "all other parameters are passed to compute program")]
		)
	}

newtype ComputeProgram = ComputeProgram String
	deriving (Show)

getComputeProgram :: RemoteConfig -> Either String ComputeProgram
getComputeProgram c = case fromProposedAccepted <$> M.lookup programField c of
	Just program
		| safetyPrefix `isPrefixOf` program ->
			Right (ComputeProgram program)
		| otherwise -> Left $
			"The program's name must begin with \"" ++ safetyPrefix ++ "\""
	Nothing -> Left "Specify program="

-- Limiting the program to "git-annex-compute-" prefix is important for
-- security, it prevents autoenabled compute remotes from running arbitrary
-- programs.
safetyPrefix :: String
safetyPrefix = "git-annex-compute-"

programField :: RemoteConfigField
programField = Accepted "program"

data ProcessCommand
	= ProcessInput FilePath
	| ProcessOutput FilePath
	| ProcessReproducible
	| ProcessProgress PercentFloat
	deriving (Show, Eq)

instance Proto.Receivable ProcessCommand where
	parseCommand "INPUT" = Proto.parse1 ProcessInput
	parseCommand "OUTPUT" = Proto.parse1 ProcessOutput
	parseCommand "REPRODUCIBLE" = Proto.parse0 ProcessReproducible
	parseCommand "PROGRESS" = Proto.parse1 ProcessProgress
	parseCommand _ = Proto.parseFail

newtype PercentFloat = PercentFloat Float
	deriving (Show, Eq)

instance Proto.Serializable PercentFloat where
	serialize (PercentFloat p) = show p
	deserialize s = PercentFloat <$> readMaybe s

data ComputeState = ComputeState
	{ computeParams :: [String]
	, computeInputs :: M.Map FilePath Key
	, computeOutputs :: M.Map FilePath (Maybe Key)
	, computeReproducible :: Bool
	}
	deriving (Show, Eq)

{- Formats a ComputeState as an URL query string.
 -
 - Prefixes computeParams with 'p', computeInputs with 'i',
 - and computeOutput with 'o'.
 -
 - When the passed Key is an output, rather than duplicate it
 - in the query string, that output has no value.
 -
 - Example: "psomefile&pdestfile&pbaz&isomefile=WORM--foo&odestfile="
 -
 - The computeParams are in the order they were given. The computeInputs
 - and computeOutputs are sorted in ascending order for stability.
 -}
formatComputeState :: Key -> ComputeState -> B.ByteString
formatComputeState k st = renderQuery False $ concat
	[ map formatparam (computeParams st)
	, map formatinput (M.toAscList (computeInputs st))
	, mapMaybe formatoutput (M.toAscList (computeOutputs st))
	]
  where
	formatparam p = ("p" <> encodeBS p, Nothing)
	formatinput (file, key) =
		("i" <> toRawFilePath file, Just (serializeKey' key))
	formatoutput (file, (Just key)) = Just $
		("o" <> toRawFilePath file,
			if key == k
				then Nothing
				else Just (serializeKey' key)
		)
	formatoutput (_, Nothing) = Nothing

parseComputeState :: Key -> B.ByteString -> Maybe ComputeState
parseComputeState k b =
	let st = go emptycomputestate (parseQuery b)
	in if st == emptycomputestate then Nothing else Just st
  where
	emptycomputestate = ComputeState mempty mempty mempty False
	go :: ComputeState -> [QueryItem] -> ComputeState
	go c [] = c { computeParams = reverse (computeParams c) }
	go c ((f, v):rest) = 
		let c' = fromMaybe c $ case decodeBS f of
			('p':p) -> Just $ c
				{ computeParams = p : computeParams c
				}
			('i':i) -> do
				key <- deserializeKey' =<< v
				Just $ c
					{ computeInputs = 
						M.insert i key
							(computeInputs c)
					}
			('o':o) -> case v of
				Just kv -> do
					key <- deserializeKey' kv
					Just $ c
						{ computeOutputs =
							M.insert o (Just key)
								(computeOutputs c)
						}
				Nothing -> Just $ c
					{ computeOutputs = 
						M.insert o (Just k)
							(computeOutputs c)
					}
			_ -> Nothing
		in go c' rest

{- The per remote metadata is used to store ComputeState. This allows
 - recording multiple ComputeStates that generate the same key.
 -
 - The metadata fields are numbers (prefixed with "t" to make them legal
 - field names), which are estimates of how long it might take to run
 - the computation (in seconds).
 -}
setComputeState :: RemoteStateHandle -> Key -> NominalDiffTime -> ComputeState -> Annex ()
setComputeState rs k ts st = addRemoteMetaData k rs $ MetaData $ M.singleton
	(mkMetaFieldUnchecked $ T.pack ('t':show (truncateResolution 1 ts)))
	(S.singleton (MetaValue (CurrentlySet True) (formatComputeState k st)))

getComputeStates :: RemoteStateHandle -> Key -> Annex [(NominalDiffTime, ComputeState)]
getComputeStates rs k = do
	RemoteMetaData _ (MetaData m) <- getCurrentRemoteMetaData rs k
	return $ go [] (M.toList m)
  where
	go c [] = concat c
	go c ((f, s) : rest) =
		let sts = mapMaybe (parseComputeState k . fromMetaValue)
			(S.toList s)
		in case parsePOSIXTime (T.encodeUtf8 (T.drop 1 (fromMetaField f))) of
			Just ts -> go (zip (repeat ts) sts : c) rest
			Nothing -> go c rest

computeProgramEnvironment :: ComputeState -> Annex [(String, String)]
computeProgramEnvironment st = do
	environ <- filter (caninherit . fst) <$> liftIO getEnvironment
	let addenv = mapMaybe go (computeParams st)
	return $ environ ++ addenv
  where
	envprefix = "ANNEX_COMPUTE_"
	caninherit v = not (envprefix `isPrefixOf` v)
	go p
		| '=' `elem` p =
			let (f, v) = separate (== '=') p
			in Just (envprefix ++ f, v)
		| otherwise = Nothing

newtype ImmutableState = ImmutableState Bool

runComputeProgram
	:: ComputeProgram
	-> ComputeState
	-> ImmutableState
	-> (OsPath -> Annex (Key, Maybe OsPath))
	-> (ComputeState -> OsPath -> Annex v)
	-> Annex v
runComputeProgram (ComputeProgram program) state (ImmutableState immutablestate) getinputcontent cont =
	withOtherTmp $ \tmpdir -> 
		go tmpdir 
			`finally` liftIO (removeDirectoryRecursive tmpdir)
  where
	go tmpdir = do
		environ <- computeProgramEnvironment state
		let pr = (proc program (computeParams state))
			 { cwd = Just (fromOsPath tmpdir)
			 , std_in = CreatePipe
			 , std_out = CreatePipe
			 , env = Just environ
			 }
		state' <- bracket
			(liftIO $ createProcess pr)
			(liftIO . cleanupProcess)
			(getinput state tmpdir)
		cont state' tmpdir
	
	getinput state' tmpdir p = 
		liftIO (hGetLineUntilExitOrEOF (processHandle p) (stdoutHandle p)) >>= \case
			Just l
				| null l -> getinput state' tmpdir p
				| otherwise -> do
					state'' <- parseoutput p state' l
					getinput state'' tmpdir p
			Nothing -> do
				liftIO $ hClose (stdoutHandle p)
				liftIO $ hClose (stdinHandle p)
				unlessM (liftIO $ checkSuccessProcess (processHandle p)) $
					giveup $ program ++ " exited unsuccessfully"
				return state'
	
	parseoutput p state' l = case Proto.parseMessage l of
		Just (ProcessInput f) ->
			let knowninput = M.member f (computeInputs state')
			in checkimmutable knowninput l $ do
				(k, mp) <- getinputcontent (toOsPath f)
				liftIO $ hPutStrLn (stdinHandle p) $
					maybe "" fromOsPath mp
				return $ if knowninput
					then state'
					else state'
						{ computeInputs = 
							M.insert f k
								(computeInputs state')
						}
		Just (ProcessOutput f) ->
			let knownoutput = M.member f (computeOutputs state')
			in checkimmutable knownoutput l $ 
				return $ if knownoutput
					then state'
					else state'
						{ computeOutputs = 
							M.insert f Nothing
								(computeOutputs state')
						}
		Just (ProcessProgress percent) -> do
			-- XXX
			return state'
		Just ProcessReproducible ->
			return $ state' { computeReproducible = True }
		Nothing -> giveup $
			program ++ " output included an unparseable line: \"" ++ l ++ "\""

	checkimmutable True _ a = a
	checkimmutable False l a
		| not immutablestate = a
		| otherwise = giveup $
			program ++ " is not behaving the same way it used to, now outputting: \"" ++ l ++ "\""

computeKey :: RemoteStateHandle -> ComputeProgram -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
computeKey rs (ComputeProgram program) k af dest p vc = do
	states <- map snd . sortOn fst -- least expensive probably
		<$> getComputeStates rs k
	case mapMaybe computeskey states of
		((keyfile, state):_) -> runComputeProgram
			(ComputeProgram program)
			state
			(ImmutableState True)
			(getinputcontent state)
			(go keyfile)
		[] -> giveup "Missing compute state"
  where
	getinputcontent state f =
		case M.lookup (fromOsPath f) (computeInputs state) of
			Just inputkey -> do
				obj <- calcRepo (gitAnnexLocation inputkey)
				-- XXX get input object when not present
				return (inputkey, Just obj)
			Nothing -> error "internal"

	computeskey state = 
		case M.keys $ M.filter (== Just k) (computeOutputs state) of
			(keyfile : _) -> Just (keyfile, state)
			[] -> Nothing

	go keyfile state tmpdir = do
		let keyfile' = tmpdir </> toOsPath keyfile
		unlessM (liftIO $ doesFileExist keyfile') $
			giveup $ program ++ " exited sucessfully, but failed to write the computed file"
		catchNonAsync (liftIO $ moveFile keyfile' dest)
			(\err -> giveup $ "failed to move the computed file: " ++ show err)
		
		-- Try to move any other computed object files into the annex.
		forM_ (M.toList $ computeOutputs state) $ \case
			(file, (Just key)) ->
				when (k /= key) $ do
					let file' = tmpdir </> toOsPath file
					whenM (liftIO $ doesFileExist file') $
						whenM (verifyKeyContentPostRetrieval RetrievalAllKeysSecure vc verification k file') $
							void $ tryNonAsync $ moveAnnex k file'
			_ -> noop

		return verification
	
	-- The program might not be reproducible, so require strong
	-- verification.
	verification = MustVerify

-- Make sure that the compute state exists.
checkKey :: RemoteStateHandle -> Key -> Annex Bool
checkKey rs k = do
	states <- getComputeStates rs k
	if null states
		then giveup "Missing compute state"
		else return True

-- Unsetting the compute state will prevent computing the key.
dropKey :: RemoteStateHandle -> Maybe SafeDropProof -> Key -> Annex ()
dropKey rs _ k = do
	RemoteMetaData _ old <- getCurrentRemoteMetaData rs k
	addRemoteMetaData k rs (modMeta old DelAllMeta)

storeKeyUnsupported :: Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
storeKeyUnsupported _ _ _ _ = giveup "transfer to compute remote not supported; use git-annex addcomputed instead"
