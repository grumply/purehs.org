module Control.With where

import Pure.Elm

import Data.Typeable

data Msg = Startup | Receive

with :: (Typeable a, Eq a) => a -> IO () -> View -> View
with a f v = run (App [Startup] [Receive] [] a update view) (a,f,v)
  where
    update Startup (_,f,_) a = f >> pure a
    update Receive (a',f,_) a
      | a == a'   = pure a
      | otherwise = f >> pure a'

    view (_,_,v) _ = v
