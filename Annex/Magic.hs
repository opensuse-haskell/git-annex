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
import Data.Char
import qualified Data.ByteString as B
#ifdef WITH_MAGICMIME
import Magic
import qualified Data.List.NonEmpty as NE
import GHC.IO.Handle.FD (handleToFd)
import Utility.Env
import qualified Utility.FileIO as F
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
getMagicMime m f =
	-- Open the file rather than passing it to magicFile to avoid
	-- issues with filename encoding on Windows.
	F.withFile f ReadMode $ \h -> do
		fd <- handleToFd h
		Just . parseMagicMime <$> magicFD m fd
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
