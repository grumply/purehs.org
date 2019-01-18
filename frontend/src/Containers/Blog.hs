module Containers.Blog where

import Pure
import Pure.Fetch

import Scope

import Data.Maybe

data B = B deriving (Eq,Ord)

fetcher view = fetch
  Fetcher
    { request = req Scope.getPostMetas ()
    , cached  = True
    , key     = B
    , ..
    }
