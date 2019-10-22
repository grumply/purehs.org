module Components.With where

import Pure.Elm

data Msg = Startup

with f v = run (App [Startup] [] [] mdl update view) (f,v)
  where
    mdl = ()
    update _ (f,_) _ = f
    view (_,v) _ = v