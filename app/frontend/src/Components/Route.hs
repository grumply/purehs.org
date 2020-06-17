module Components.Route where

import Data.Render
import Data.Route

import Pure.Elm.Application

instance Render Route where
  render rt = A <| link rt