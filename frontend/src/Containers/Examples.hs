module Containers.Examples where

import Pure
import Pure.Fetch

import Scope

import Data.Maybe

data E = E deriving (Eq,Ord)

fetcher view = fetch
  Fetcher
    { request = req Scope.getExamples ()
    , cached  = True
    , key     = E
    , ..
    }
