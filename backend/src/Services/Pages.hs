module Services.Pages (Pages(),MonadPages(..),productionPages) where

import Pure.Capability
import Pure.Capability.TH
import Pure.Data.Txt as Txt

import Data.IORef
import Data.List as List
import Data.Maybe
import System.IO.Unsafe

import Services.Shared
import Shared (Page(..),PageMeta(..))

{-# NOINLINE pages #-}
pages :: IORef [Page]
pages = unsafePerformIO $ newIORef []

data Pages m = Pages
  { _loadPages :: m ()
  , _lookupPage :: Txt -> m (Maybe Page)
  }
mkCapability ''Pages

productionPages :: forall m. MonadIO m => Pages m
productionPages = Pages {..}
  where
    _loadPages :: m ()
    _loadPages = liftIO $ do
      ps <- load "pages" $ \fn -> Page (PageMeta fn)
      -- note we store these in most-recent-first order, hence the reverse
      writeIORef pages (sortOn meta ps)

    _lookupPage :: Txt -> m (Maybe Page)
    _lookupPage p = do
      ps <- liftIO (readIORef pages)
      let matches = List.filter ((p ==) . slug . meta) ps
      pure $ listToMaybe matches

