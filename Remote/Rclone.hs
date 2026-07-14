{- Rclone special remote, using "rclone gitannex"
 -
 - Copyright 2024-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Rclone (remote) where

import Types
import Types.Remote
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Utility.SafeCommand
import qualified Remote.External as External
import Remote.External.Types

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "rclone"
	, enumerate = const (findSpecialRemotes "rclone")
	, generate = External.gen remote p
	, configParser = External.remoteConfigParser p
	, setup = External.externalSetup p setgitconfig 
	, exportSupported = External.checkSupportedWith p External.checkExportSupported
	, importSupported = External.checkSupportedWith p External.checkImportSupported
	, exportImportSupported = exportImportUnsupported
	, thirdPartyPopulated = False
	}
  where
	p = Just $ ExternalCommand "rclone" [Param "gitannex"]
	setgitconfig = Just ("rclone", "true")
