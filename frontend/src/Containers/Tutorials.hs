module Containers.Tutorials where

import Pure
import Pure.Fetch

import Scope

import Data.Maybe

data T = T deriving (Eq,Ord)

fetcher view = fetch
  Fetcher
    { request = req Scope.getTutorialMetas ()
    , cached  = True
    , key     = T
    , ..
    }
