{- Adds readonly support to remotes.
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.ReadOnly
	( adjustReadOnly
	, readonlyStoreKey
	, readonlyStorer
	, readonlyRemoveKey
	) where

import Annex.Common
import Types.Remote
import Types.StoreRetrieve
import Utility.Metered

{- Adds support for read-only remotes, by replacing the
 - methods that write to a remote with dummies that fail.
 -
 - Note that disabling git pushes to remotes is not handled here.
 -}
adjustReadOnly :: Remote -> Remote
adjustReadOnly r
	| remoteAnnexReadOnly (gitconfig r) = r
		{ storeKey = readonlyStoreKey
		, removeKey = readonlyRemoveKey
		, repairRepo = Nothing
		, exportActions = exportActions r
			{ storeExport = readonlyStoreExport
			, removeExport = readonlyRemoveExport
			, removeExportDirectory = Just readonlyRemoveExportDirectory
			, renameExport = readonlyRenameExport
			}
		, importActions = importActions r
			{ storeExportWithContentIdentifier = readonlyStoreExportWithContentIdentifiera
			, removeExportWithContentIdentifier = readonlyRemoveExportWithContentIdentifier
			, removeExportDirectoryWhenEmpty = Just readonlyRemoveExportDirectory
			}
		}
	| otherwise = r

readonlyStoreKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
readonlyStoreKey _ _ _ = readonlyFail

readonlyRemoveKey :: Key -> Annex Bool
readonlyRemoveKey _ = readonlyFail

readonlyStorer :: Storer
readonlyStorer _ _ _ = readonlyFail

readonlyStoreExport :: FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool
readonlyStoreExport _ _ _ _ = readonlyFail

readonlyRemoveExport :: Key -> ExportLocation -> Annex Bool
readonlyRemoveExport _ _ = readonlyFail

readonlyRemoveExportDirectory :: ExportDirectory -> Annex Bool
readonlyRemoveExportDirectory _ = readonlyFail

readonlyRenameExport :: Key -> ExportLocation -> ExportLocation -> Annex (Maybe Bool)
readonlyRenameExport _ _ _ = return Nothing

readonlyStoreExportWithContentIdentifier :: FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex (Maybe ContentIdentifier)
readonlyStoreExportWithContentIdentifier _ _ _ _ _ = do
	readonlyWarning
	return Nothing

removeExportWithContentIdentifier :: Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
removeExportWithContentIdentifier _ _ _ = readonlyFail

readonlyFail :: Annex Bool
readonlyFail = do
	readonlyWarning
	return False

readonlyWarning :: Annex ()
readonylWarning = warning "this remote is readonly"
