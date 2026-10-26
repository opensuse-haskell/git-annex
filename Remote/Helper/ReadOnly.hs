{- Adds readonly support to remotes.
 -
 - Copyright 2013-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.ReadOnly
	( adjustReadOnly
	, forceReadOnly
	, readonlyStoreKey
	, readonlyStorer
	, readonlyRemoveKey
	, readonlyWarning
	) where

import Annex.Common
import Types.Remote
import Types.StoreRetrieve
import Types.Import
import Types.Export
import Utility.Metered

{- Adds support for read-only remotes, by replacing the
 - methods that write to a remote with dummies that fail.
 -
 - Note that disabling git pushes to remotes is not handled here.
 -}
adjustReadOnly :: Remote -> Remote
adjustReadOnly r
	| remoteAnnexReadOnly (gitconfig r) = makeReadOnly readonlyWarning r
	| otherwise = r

{- Force a remote that would not usually be read-only to be read-only,
 - with a message explaining why. -}
forceReadOnly :: String -> Remote -> Remote
forceReadOnly msg r
	| remoteAnnexReadOnly (gitconfig r) = makeReadOnly readonlyWarning r
	| otherwise = makeReadOnly msg r

makeReadOnly :: String -> Remote -> Remote
makeReadOnly msg r = r
	{ storeKey = readonlyStoreKey msg
	, removeKey = readonlyRemoveKey msg
	, repairRepo = Nothing
	, exportActions = (exportActions r)
		{ storeExport = readonlyStoreExport msg
		, removeExport = readonlyRemoveExport msg
		, removeExportDirectory = Just (readonlyRemoveExportDirectory msg)
		, renameExport = Nothing
		}
	, exportImportActions = (exportImportActions r)
		{ storeExportWithContentIdentifier = readonlyStoreExportWithContentIdentifier msg
		, removeExportWithContentIdentifier = readonlyRemoveExportWithContentIdentifier msg
		, removeExportDirectoryWhenEmpty = Just (readonlyRemoveExportDirectory msg)
		}
	}

readonlyStoreKey :: String -> Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
readonlyStoreKey msg _ _ _ _ = readonlyFail msg

readonlyRemoveKey :: String -> Maybe SafeDropProof -> Key -> Annex ()
readonlyRemoveKey msg _ _ = readonlyFail msg

readonlyStorer :: String -> Storer
readonlyStorer msg _ _ _ = readonlyFail msg

readonlyStoreExport :: String -> OsPath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
readonlyStoreExport msg _ _ _ _ = readonlyFail msg

readonlyRemoveExport :: String -> Key -> ExportLocation -> Annex ()
readonlyRemoveExport msg _ _ = readonlyFail msg

readonlyRemoveExportDirectory :: String -> ExportDirectory -> Annex ()
readonlyRemoveExportDirectory msg _ = readonlyFail msg

readonlyStoreExportWithContentIdentifier :: String -> OsPath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
readonlyStoreExportWithContentIdentifier msg _ _ _ _ _ = readonlyFail msg

readonlyRemoveExportWithContentIdentifier :: String -> Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
readonlyRemoveExportWithContentIdentifier msg _ _ _ = readonlyFail msg

readonlyFail :: String -> Annex a
readonlyFail = giveup

readonlyWarning :: String
readonlyWarning = "this remote is readonly"
