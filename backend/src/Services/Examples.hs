module Services.Examples (Examples(),MonadExamples(..),productionExamples) where

import Pure.Capability
import Pure.Capability.TH

import Data.IORef
import Data.List
import System.IO.Unsafe

import Services.Shared
import Shared (Example(..),ExampleMeta(..))

{-# NOINLINE examples #-}
examples :: IORef [Example]
examples = unsafePerformIO $ newIORef []

data Examples m = Examples
  { _loadExamples :: m ()
  , _getExamples :: m [Example]
  }
mkCapability ''Examples

productionExamples :: forall m. MonadIO m => Examples m
productionExamples = Examples {..}
  where
    _loadExamples :: m ()
    _loadExamples = liftIO $ do
      es <- load "examples" $ \fn ->
        let num `Dash` slug = fn
        in Example ExampleMeta {..}
      writeIORef examples (sortOn meta es)

    _getExamples :: m [Example]
    _getExamples = liftIO (readIORef examples)

