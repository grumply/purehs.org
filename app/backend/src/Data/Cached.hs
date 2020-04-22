module Data.Cached (Cached,cache,forkCache,cached) where

import Pure.Data.Time

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Function
import System.Mem.Weak

type Duration = Time

data Cached a = Cached
  { construct :: IO (Time,a)
  , value     :: MVar (Time,a)
  , expensive :: Bool
  }

cache_ :: Bool -> Duration -> IO a -> IO (Cached a)
cache_ expensive d ma = do
  let construct = (,) <$> ((+ d) <$> time) <*> ma
  value <- newMVar =<< construct
  _value <- mkWeakMVar value (pure ())
  let
    worker = do
      threadDelay (round (getMillis (getTime d)) * 1000)
      mvalue <- deRefWeak _value
      case mvalue of
        Nothing -> pure ()
        Just value -> do
          tma <- construct
          void $ swapMVar value tma
          worker
  when expensive $ void $ forkIO worker
  pure Cached {..}

cache :: Duration -> IO a -> IO (Cached a)
cache = cache_ False

forkCache :: Duration -> IO a -> IO (Cached a)
forkCache = cache_ True

cached :: Cached a -> IO a
cached Cached {..} = time >>= \now -> modifyMVar value $ \case
  v | now > fst v
    , not expensive -> construct >>= \v -> pure (v,snd v)
    | otherwise     -> pure (v,snd v)
