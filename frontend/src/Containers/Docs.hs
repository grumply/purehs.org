module Containers.Docs where

import Pure
import Pure.Fetch

import Scope

import Data.Maybe

data D = D deriving (Eq,Ord)

fetcher view = fetch
  Fetcher
    { request = req Scope.getDocMetas ()
    , cached  = True
    , key     = D
    , ..
    }
