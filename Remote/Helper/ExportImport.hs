{- Helper to make remotes support export and import (or not).
 -
 - Copyright 2017-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Remote.Helper.ExportImport where

import Annex.Common
import qualified Annex
import Types.Remote
import Types.Key
import Types.ProposedAccepted
import Remote.Helper.Encryptable (encryptionIsEnabled)
import qualified Database.Export as Export
import qualified Database.ContentIdentifier as ContentIdentifier
import Annex.Export
import Annex.LockFile
import Annex.SpecialRemote.Config
import Annex.Verify
import Annex.Content
import Git.Types (fromRef)
import Logs.Export
import Logs.ContentIdentifier (recordContentIdentifier)

import Control.Concurrent.STM

-- | Use for remotes that do not support exports.
class HasExportUnsupported a where
	exportUnsupported :: a

instance HasExportUnsupported (ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool) where
	exportUnsupported = \_ _ -> return False

instance HasExportUnsupported (ExportActions Annex) where
	exportUnsupported = ExportActions
		{ storeExport = nope
		, retrieveExport = nope
		, checkPresentExport = \_ _ -> return False
		, removeExport = nope
		, removeExportDirectory = nope
		, renameExport = Nothing
		}
	 where
	 	nope = giveup "export not supported"

-- | Use for remotes that do not support imports.
class HasImportUnsupported a where
	importUnsupported :: a

instance HasImportUnsupported (ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool) where
	importUnsupported = \_ _ -> return False

instance HasImportUnsupported (ImportActions Annex) where
	importUnsupported = ImportActions
		{ listImportableContents = nope
		, importKey = Nothing
		, retrieveExportWithContentIdentifier = nope
		, storeExportWithContentIdentifier = nope
		, removeExportWithContentIdentifier = nope
		, removeExportDirectoryWhenEmpty = nope
		, checkPresentExportWithContentIdentifier = \_ _ _ -> return False
		}
	  where
		nope = giveup "import not supported"

exportIsSupported :: ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool
exportIsSupported = \_ _ -> return True

importIsSupported :: ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool
importIsSupported = \_ _ -> return True

-- | Prevent or allow exporttree=yes and importtree=yes when
-- setting up a new remote, depending on the remote's capabilities.
adjustExportImportRemoteType :: RemoteType -> RemoteType
adjustExportImportRemoteType rt = rt { setup = setup' }
  where
	setup' st mu cp c gc = do
		pc <- either giveup return . parseRemoteConfig c
			=<< configParser rt c
		let checkconfig supported configured configfield cont =
			ifM (supported rt pc gc <&&> pure (not (thirdPartyPopulated rt)))
				( case st of
					Init
						| configured pc && encryptionIsEnabled pc ->
							giveup $ "cannot enable both encryption and " ++ fromProposedAccepted configfield
						| otherwise -> cont
					Enable oldc -> enable oldc pc configured configfield cont
					AutoEnable oldc -> enable oldc pc configured configfield cont
				, if configured pc
					then giveup $ fromProposedAccepted configfield ++ " is not supported by this special remote"
					else cont
				)
		checkconfig exportSupported exportTree exportTreeField $
			checkconfig importSupported importTree importTreeField $
				setup rt st mu cp c gc
	
	enable oldc pc configured configfield cont = do
		oldpc <- parsedRemoteConfig rt oldc
		if configured pc /= configured oldpc
			then giveup $ "cannot change " ++ fromProposedAccepted configfield ++ " of existing special remote"
			else cont

-- | Adjust a remote to support exporttree=yes and/or importree=yes.
adjustExportImport :: Remote -> RemoteStateHandle -> Annex Remote
adjustExportImport r rs = do
	isexport <- pure (exportTree (config r))
		<&&> isExportSupported r
	-- When thirdPartyPopulated is True, the remote
	-- does not need to be configured with importTree to support
	-- imports.
	isimport <- pure (importTree (config r) || thirdPartyPopulated (remotetype r))
		<&&> isImportSupported r
	let r' = r
		{ remotetype = (remotetype r)
			{ exportSupported = if isexport
				then exportSupported (remotetype r)
				else exportUnsupported
			, importSupported = if isimport
				then importSupported (remotetype r)
				else importUnsupported
			}
		}
	let annexobjects = isexport && annexObjects (config r)
	if not isexport && not isimport
		then return r'
		else do
			gc <- Annex.getGitConfig
			adjustExportImport' isexport isimport annexobjects r' rs gc

adjustExportImport' :: Bool -> Bool -> Bool -> Remote -> RemoteStateHandle -> GitConfig -> Annex Remote
adjustExportImport' isexport isimport annexobjects r rs gc = do
	dbv <- prepdbv
	ciddbv <- prepciddb
	return $ r
		{ exportActions = if isexport
			then if isimport
				then exportActionsForImport dbv ciddbv (exportActions r)
				else exportActions r
			else exportUnsupported
		, importActions = if isimport
			then importActions r
			else importUnsupported
		, storeKey = \k af o p ->
			-- Storing a key to an export location could be
			-- implemented, but it would perform unnecessary work
			-- when another repository has already stored the
			-- key, and the local repository does not know
			-- about it. To avoid unnecessary costs, don't do it.
			if thirdpartypopulated
				then giveup "remote is not populated by git-annex"
				else if isexport
					then if annexobjects
						then storeannexobject k o p
						else giveup "remote is configured with exporttree=yes; use `git-annex export` to store content on it"
					else if isimport
						then giveup "remote is configured with importtree=yes and without exporttree=yes; cannot modify content stored on it"
						else storeKey r k af o p
		, removeKey = \proof k -> 
			-- Removing a key from an export location would need
			-- to change the tree in the export log to not include
			-- the file. Otherwise, conflicts when removing
			-- files would not be dealt with correctly.
			-- There does not seem to be a good use case for
			-- removing a key from an exported tree in any case.
			if thirdpartypopulated
				then giveup "dropping content from this remote is not supported"
				else if isexport
					then if annexobjects
						then removeannexobject dbv k
						else giveup "dropping content from an export is not supported; use `git annex export` to export a tree that lacks the files you want to remove"
					else if isimport
						then giveup "dropping content from this remote is not supported because it is configured with importtree=yes"
						else removeKey r proof k
		, lockContent = if versioned
			then lockContent r
			else Nothing
		, retrieveKeyFile = \k af dest p vc ->
			if isimport || isexport
				then supportversionedretrieve k af dest p vc $
					supportretrieveannexobject dbv k af dest p $
						retrieveFromImportOrExport (tryexportlocs dbv k) ciddbv k af dest p
				else retrieveKeyFile r k af dest p vc
		, retrieveKeyFileCheap = if versioned
			then retrieveKeyFileCheap r
			else Nothing
		, checkPresent = \k -> if versioned
			then checkPresent r k
			else if isimport
				then checkpresentwith k $
					anyM (checkPresentImport ciddbv k)
						=<< getanyexportlocs dbv k
				else if isexport
					-- Check if any of the files a key
					-- was exported to are present. This 
					-- doesn't guarantee the export
					-- contains the right content,
					-- if the remote is an export,
					-- or if something else can write
					-- to it. Remotes that have such 
					-- problems are made untrusted,
					-- so it's not worried about here.
					then checkpresentwith k $
						anyM (checkPresentExport (exportActions r) k)
							=<< getanyexportlocs dbv k
					else checkPresent r k
		-- checkPresent from an export is more expensive
		-- than otherwise, so not cheap. Also, this
		-- avoids things that look at checkPresentCheap and
		-- silently skip non-present files from behaving
		-- in confusing ways when there's an export
		-- conflict (or an import conflict).
		, checkPresentCheap = False
		-- Export/import remotes can lose content stored on them in
		-- many ways. This is not a problem with versioned
		-- ones though, since they still allow accessing by Key.
		-- And for thirdPartyPopulated, it depends on how the
		-- content gets actually stored in the remote, so
		-- is not overridden here.
		, untrustworthy =
			if versioned || thirdPartyPopulated (remotetype r)
				then untrustworthy r
				else True
		-- git-annex testremote cannot be used to test
		-- import/export since it stores keys.
		, mkUnavailable = return Nothing
		, getInfo = do
			is <- getInfo r
			is' <- if isexport && not thirdpartypopulated
				then do
					ts <- map fromRef . exportedTreeishes
						<$> getExport (uuid r)
					return $ is ++ catMaybes
						[ Just ("exporttree", "yes")
						, Just ("exportedtree", unwords ts)
						, if annexobjects
							then Just ("annexobjects", "yes")
							else Nothing
						]
				else return is
			return $ if isimport && not thirdpartypopulated
				then (is'++[("importtree", "yes")])
				else is'
		}
  where
	versioned = isVersioning (config r)
	
	thirdpartypopulated = thirdPartyPopulated (remotetype r)

	-- exportActions adjusted to use the equivalent import actions,
	-- which take ContentIdentifiers into account.
	exportActionsForImport dbv ciddbv ea = ea
  		{ storeExport = \f k loc p -> do
			db <- getciddb ciddbv
			exportdb <- getexportdb dbv
			oldks <- liftIO $ Export.getExportTreeKey exportdb loc
			oldcids <- liftIO $ concat
				<$> mapM (ContentIdentifier.getContentIdentifiers db rs) oldks
			newcid <- storeExportWithContentIdentifier (importActions r) f k loc oldcids p
			cidlck <- calcRepo' gitAnnexContentIdentifierLock
			withExclusiveLock cidlck $ do
				liftIO $ ContentIdentifier.recordContentIdentifier db rs newcid k
				liftIO $ ContentIdentifier.flushDbQueue db
			recordContentIdentifier rs newcid k
		, removeExport = \k loc ->
			removeExportWithContentIdentifier (importActions r) k loc
				=<< getkeycids ciddbv k
		, removeExportDirectory = removeExportDirectoryWhenEmpty (importActions r)
		-- renameExport is optional, and the remote's
		-- implementation may lose modifications to the file
		-- (by eg copying and then deleting) so don't use it
		, renameExport = Nothing
		, checkPresentExport = checkPresentImport ciddbv
		}
	
	prepciddb = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbtv <- liftIO newEmptyTMVarIO
		return (dbtv, lcklckv)
	
	prepdbv = do
		lcklckv <- liftIO newEmptyTMVarIO
		dbv <- liftIO newEmptyTMVarIO
		exportinconflict <- liftIO $ newTVarIO False
		return (dbv, lcklckv, exportinconflict)

	-- Only open the database once it's needed.
	getciddb (dbtv, lcklckv) =
		liftIO (atomically (tryReadTMVar dbtv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- ContentIdentifier.openDb
					db' <- ContentIdentifier.needsUpdateFromLog db >>= \case
						Just v -> do
							cidlck <- calcRepo' gitAnnexContentIdentifierLock 
							withExclusiveLock cidlck $
								ContentIdentifier.updateFromLog db v
						Nothing -> pure db
					liftIO $ atomically $ putTMVar dbtv db'
					return db'
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbtv)
				)
	
	-- Only open the database once it's needed.
	--
	-- After opening the database, check if the export log is
	-- different than the database, and update the database, to notice
	-- when an export has been updated from another repository.
	getexportdb (dbv, lcklckv, exportinconflict) = 
		liftIO (atomically (tryReadTMVar dbv)) >>= \case
			Just db -> return db
			-- let only one thread take the lock
			Nothing -> ifM (liftIO $ atomically $ tryPutTMVar lcklckv ())
				( do
					db <- Export.openDb (uuid r)
					updateexportdb db exportinconflict
					liftIO $ atomically $ putTMVar dbv db
					return db
				-- loser waits for winner to open the db and
				-- can then also use its handle
				, liftIO $ atomically (readTMVar dbv)
				)

	isexportinconflict (_, _, v) = liftIO $ atomically $ readTVar v

	updateexportdb db exportinconflict =
		Export.updateExportTreeFromLog db >>= \case
			Export.ExportUpdateSuccess -> return ()
			Export.ExportUpdateConflict -> do
				warnExportImportConflict r
				liftIO $ atomically $
					writeTVar exportinconflict True
		
	getanyexportlocs dbv k = do
		db <- getexportdb dbv
		liftIO $ Export.getExportTree db k
	
	getexportlocs dbv k = do
		db <- getexportdb dbv
		liftIO (Export.getExportTree db k) >>= \case
			[] -> ifM (isexportinconflict dbv)
				( giveup "unknown export location, likely due to the export conflict"
				, return []
				)
			ls -> return ls
	
	tryexportlocs dbv k a = 
		go Nothing =<< getexportlocs dbv k
	  where
		go Nothing [] = giveup "unknown export location"
		go (Just ex) [] = throwM ex
		go _ (l:ls) = tryNonAsync (a l) >>= \case
			Right v -> return v
			Left e -> go (Just e) ls
		
	getkeycids ciddbv k = do
		db <- getciddb ciddbv
		liftIO $ ContentIdentifier.getContentIdentifiers db rs k

	retrieveFromImportOrExport getlocs ciddbv k af dest p
		| isimport = retrieveFromImport getlocs ciddbv k af dest p
		| otherwise = retrieveFromExport getlocs k af dest p

	-- Keys can be retrieved using retrieveExport, but since that
	-- retrieves from a path in the remote that another writer could
	-- have replaced with content not of the requested key, the content
	-- has to be strongly verified.
	retrieveFromExport getlocs k _af dest p = ifM (isVerifiable k)
		( getlocs $ \loc -> 
			retrieveExport (exportActions r) k loc dest p >>= return . \case
				UnVerified -> MustVerify
				IncompleteVerify iv -> MustFinishIncompleteVerify iv
				v -> v
		, giveup $ "exported content cannot be verified due to using the " ++ decodeBS (formatKeyVariety (fromKey keyVariety k)) ++ " backend"
		)
	
	retrieveFromImport getlocs ciddbv k af dest p = do
		cids <- getkeycids ciddbv k
		if not (null cids)
			then getlocs $ \loc ->
				snd <$> retrieveExportWithContentIdentifier (importActions r) loc cids dest (Left k) p
			-- In case a content identifier is somehow missing,
			-- try this instead.
			else if isexport
				then retrieveFromExport getlocs k af dest p
				else giveup "no content identifier is recorded, unable to retrieve"

	checkpresentwith k a = ifM a
		( return True
		, if annexobjects
			then checkpresentannexobject k
			else return False
		)

	checkPresentImport ciddbv k loc =
		checkPresentExportWithContentIdentifier
			(importActions r)
			k loc 
			=<< getkeycids ciddbv k

	annexobjectlocation k = exportAnnexObjectLocation gc k

	checkpresentannexobject k = 
		checkPresentExport (exportActions r) k (annexobjectlocation k)

	storeannexobject k o p = prepSendAnnex' k o >>= \case
		Nothing -> giveup "content is not available"
		Just (src, _, checkmodified) -> do
			let loc = annexobjectlocation k
			storeExport (exportActions r) src k loc p
			checkmodified >>= \case
				Nothing -> return ()
				Just err -> do
					removeExport (exportActions r) k loc
					giveup err

	removeannexobject dbv k = 
		getanyexportlocs dbv k >>= \case
			[] -> ifM (isexportinconflict dbv)
				( do
					warnExportImportConflict r
					giveup "Cannot remove content from the remote until the conflict has been resolved."
				, removeExport (exportActions r) k (annexobjectlocation k)
				)
			_ -> giveup "This key is part of the exported tree, so can only be removed by exporting a tree that does not include it."

	retrieveannexobject k af dest p =
		retrieveFromExport getlocs k af dest p
	  where
		getlocs a = a (annexobjectlocation k)

	supportretrieveannexobject dbv k af dest p a
		| annexobjects = tryNonAsync a >>= \case
			Right res -> return res
			Left err -> tryNonAsync (retrieveannexobject k af dest p) >>= \case
				Right res -> return res
				-- Both failed, so which exception to
				-- throw? If there are known export
				-- locations, throw the exception from
				-- retrieving from the export locations.
				-- If there are no known export locations,
				-- throw the exception from retrieving from
				-- the annexobjects location.
				Left err' -> getanyexportlocs dbv k >>= \case
					[] -> ifM (isexportinconflict dbv)
						( throwM err
						, throwM err'
						)
					_ -> throwM err
		| otherwise = a

	-- versioned remotes have a key/value store which
	-- the usual retrieveKeyFile can be used with, rather than
	-- an import/export variant. However, fall back to that
	-- if retrieveKeyFile fails.
	supportversionedretrieve k af dest p vc a
		| versioned =
			retrieveKeyFile r k af dest p vc
				`catchNonAsync` const a
		| otherwise = a
