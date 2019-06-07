module Services.Storage where

import Data.Map as Map

import Pure.Data.JSON

import qualified Pure.Data.LocalStorage as LS

import Imports

data Storage m = Storage
  { _provide :: forall a. ToJSON   a => Txt -> a -> m () 
  , _remove  ::                         Txt -> m ()
  , _consume :: forall a. FromJSON a => Txt -> m (Maybe a)
  }
mkCapability ''Storage

productionStorage :: MonadIO m => Storage m
productionStorage = Storage {..}
  where
    _provide k v = liftIO $ void ( LS.put k v )
    _remove  k   = liftIO $ LS.delete k
    _consume k   = liftIO $ LS.get k

newtype Store = Store { store :: Map Txt Value }

mockStorage :: forall m. MonadIO m => IORef Store -> Storage m
mockStorage st_ = Storage {..}
  where
    _provide :: forall a. ToJSON a => Txt -> a -> m ()
    _provide k v = liftIO $ modifyIORef st_ (Store . Map.insert k (toJSON v) . store)

    _remove :: Txt -> m ()
    _remove k = liftIO $ modifyIORef st_ (Store . Map.delete k . store)

    _consume :: forall a. FromJSON a => Txt -> m (Maybe a)
    _consume k = liftIO $ do
      Store store <- readIORef st_
      return $
        case fmap fromJSON ( Map.lookup k store ) of 
          Just (Success a) -> Just a
          _                -> Nothing

