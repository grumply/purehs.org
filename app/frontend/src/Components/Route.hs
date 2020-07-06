module Components.Route where

import Components.Preload
import Data.Render
import Data.Route

import Pure.Elm.Application

instance Render Route where
  render rt = A <| prelink rt