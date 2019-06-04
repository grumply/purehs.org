module Services.Tutorials (Tutorials(),MonadTutorials(..),productionTutorials) where

import Pure.Capability
import Pure.Capability.TH
import Pure.Data.Txt as Txt

import Data.IORef
import Data.List as List
import Data.Maybe
import System.IO.Unsafe

import Services.Shared
import Shared (Tutorial(..),TutorialMeta(..))

{-# NOINLINE tutorials #-}
tutorials :: IORef [Tutorial]
tutorials = unsafePerformIO $ newIORef []

data Tutorials m = Tutorials
  { _loadTutorials    :: m ()
  , _lookupTutorial   :: Txt -> m (Maybe Tutorial)
  , _getTutorialMetas :: m [TutorialMeta]
  }
mkCapability ''Tutorials

productionTutorials :: forall m. MonadIO m => Tutorials m
productionTutorials = Tutorials {..}
  where
    _loadTutorials :: m ()
    _loadTutorials = liftIO $ do
      ts <- load "tutorials" $ \fn ->
        let number `Dash` slug = fn
            title = Txt.toTitle slug
        in Tutorial TutorialMeta {..}
      writeIORef tutorials (sortOn meta ts)

    _lookupTutorial :: Txt -> m (Maybe Tutorial)
    _lookupTutorial s = do
      ts <- liftIO (readIORef tutorials)
      let matches = List.filter ((s ==) . slug . meta) ts
      pure $ listToMaybe matches

    _getTutorialMetas :: m [TutorialMeta]
    _getTutorialMetas = do
      ts <- liftIO (readIORef tutorials)
      pure (fmap meta ts)
