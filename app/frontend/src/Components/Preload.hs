module Components.Preload where

import Pure.Elm.Application
import Data.Route
import Data.Resource

prelink r = 
  let ref = location r
  in link r . OnMouseOver (\_ -> preload ref) . OnTouchStart (\_ -> preload ref)

preload (External _) = pure ()
preload (Internal ref) = do
  mr <- route routes ref
  case mr of
    Nothing -> pure ()
    Just r  -> void (load r)

