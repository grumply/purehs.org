module Services.Docs (Docs(),MonadDocs(..),productionDocs) where

import Pure.Capability
import Pure.Capability.TH
import Pure.Data.Txt as Txt

import Data.IORef
import Data.List as List
import Data.Maybe
import System.IO.Unsafe

import Services.Shared
import Shared (Doc(..),DocMeta(..))

{-# NOINLINE docs #-}
docs :: IORef [Doc]
docs = unsafePerformIO $ newIORef []

data Docs m = Docs
  { _loadDocs    :: m ()
  , _lookupDoc   :: DocMeta -> m (Maybe Doc)
  , _getDocMetas :: m [DocMeta]
  } 
mkCapability ''Docs

productionDocs :: forall m. MonadIO m => Docs m
productionDocs = Docs {..}
  where
    _loadDocs :: m ()
    _loadDocs = liftIO $ do
      ds <- load "docs" $ \fn ->
        let (Txt.reverse -> version) `Dash` (Txt.reverse -> package) = Txt.reverse fn
        in Doc DocMeta {..}
      writeIORef docs (sortOn meta ds)

    _lookupDoc :: DocMeta -> m (Maybe Doc)
    _lookupDoc dm = do
      ds <- liftIO (readIORef docs)
      let matches = List.filter ((dm ==) . meta) ds
      pure $ listToMaybe matches

    _getDocMetas :: m [DocMeta]
    _getDocMetas = do
      ds <- liftIO (readIORef docs)
      pure (fmap meta ds)

    
