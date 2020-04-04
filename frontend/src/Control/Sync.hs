module Control.Sync (sync, sync', fork) where

import Control.Concurrent
import Control.Monad

-- synchronize a synchronously dispatched asynchronous callback with the
-- calling context through a functional reification of putMVar/takeMVar.
--
-- > result <- sync (request myAPI myClient myEndpoint myPayload)
--
-- Note: discards any return result from the outer context
sync :: ((a -> IO ()) -> IO r) -> IO a
sync f = snd <$> sync' f

-- synchronize a synchronously dispatched asynchronous callback with the
-- calling context through a functional reification of putMVar/takeMVar.
--
-- > (x,result) <- sync' (request myAPI myClient myEndpoint myPayload)
--
sync' :: ((a -> IO ()) -> IO r) -> IO (r, a)
sync' f = do
  mv <- newEmptyMVar
  r  <- f (void . tryPutMVar mv)
  a  <- readMVar mv
  pure (r, a)

fork :: IO a -> IO ()
fork = void . forkIO . void
