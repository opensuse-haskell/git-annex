{- unparsed preferred content expressions
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent.Raw where

import Annex.Common
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Logs.MapLog
import Types.StandardGroups
import Types.Group

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

{- Changes the preferred content configuration of a remote. -}
preferredContentSet :: UUID -> PreferredContentExpression -> Annex ()
preferredContentSet = setLog preferredContentLog

requiredContentSet :: UUID -> PreferredContentExpression -> Annex ()
requiredContentSet = setLog requiredContentLog

setLog :: FilePath -> UUID -> PreferredContentExpression -> Annex ()
setLog logfile uuid@(UUID _) val = do
	c <- liftIO currentVectorClock
	Annex.Branch.change logfile $
		buildLog (byteString . encodeBS)
		. changeLog c uuid val
		. parseLog Just . decodeBL
	Annex.changeState $ \s -> s 
		{ Annex.preferredcontentmap = Nothing
		, Annex.requiredcontentmap = Nothing
		}
setLog _ NoUUID _ = error "unknown UUID; cannot modify"

{- Changes the preferred content configuration of a group. -}
groupPreferredContentSet :: Group -> PreferredContentExpression -> Annex ()
groupPreferredContentSet g val = do
	c <- liftIO currentVectorClock
	Annex.Branch.change groupPreferredContentLog $
		buildGroupPreferredContent
		. changeMapLog c g val 
		. parseGroupPreferredContent
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Nothing }

parseGroupPreferredContent :: L.ByteString -> MapLog Group String
parseGroupPreferredContent = parseMapLog (Just . toGroup) Just . decodeBL

buildGroupPreferredContent :: MapLog Group PreferredContentExpression -> Builder
buildGroupPreferredContent = buildMapLog buildgroup buildexpr
  where
	buildgroup (Group g) = byteString g
	buildexpr = byteString . encodeBS

preferredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
preferredContentMapRaw = simpleMap . parseLog Just . decodeBL
	<$> Annex.Branch.get preferredContentLog

requiredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
requiredContentMapRaw = simpleMap . parseLog Just . decodeBL
	<$> Annex.Branch.get requiredContentLog

groupPreferredContentMapRaw :: Annex (M.Map Group PreferredContentExpression)
groupPreferredContentMapRaw = simpleMap . parseGroupPreferredContent
	<$> Annex.Branch.get groupPreferredContentLog
