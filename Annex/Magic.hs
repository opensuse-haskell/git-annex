{- Interface to libmagic
 -
 - Copyright 2019-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Annex.Magic (
	Magic,
	MimeType,
	MimeEncoding,
	initMagicMime,
	getMagicMime,
	getMagicMimeType,
	getMagicMimeEncoding,
	parseMagicMime,
) where

import Common
import Types.Mime
import Control.Monad.IO.Class
import qualified Data.ByteString as B
#ifdef WITH_MAGICMIME
import Magic
import Utility.Env
import Data.Char
import qualified Data.List.NonEmpty as NE
#else
type Magic = ()
#endif

initMagicMime :: IO (Maybe Magic)
#ifdef WITH_MAGICMIME
initMagicMime = catchMaybeIO $ do
	m <- magicOpen [MagicMime]
	liftIO $ getEnv "GIT_ANNEX_DIR" >>= \case
		Nothing -> magicLoadDefault m
		Just d -> magicLoad m $ NE.singleton $
			toOsPath d
				</> literalOsPath "magic"
				</> literalOsPath "magic.mgc"
	return m
#else
initMagicMime = return Nothing
#endif

getMagicMime :: Magic -> OsPath -> IO (Maybe (MimeType, MimeEncoding))
#ifdef WITH_MAGICMIME
getMagicMime m f = Just . parseMagicMime <$> magicFile m f
#else
getMagicMime _ _ = return Nothing
#endif

parseMagicMime :: B.ByteString -> (MimeType, MimeEncoding)
parseMagicMime b = (decodeBS mimetype, decodeBS mimeencoding)
  where
	(mimetype, rest) = B.break (== semicolon) b
	mimeencoding = fromMaybe mempty (B.stripPrefix "; charset=" rest)
	semicolon = fromIntegral (ord ';')

getMagicMimeType :: MonadIO m => Magic -> OsPath -> m (Maybe MimeType)
getMagicMimeType m f = liftIO $ fmap fst <$> getMagicMime m f

getMagicMimeEncoding :: MonadIO m => Magic -> OsPath -> m(Maybe MimeEncoding)
getMagicMimeEncoding m f = liftIO $ fmap snd <$> getMagicMime m f
