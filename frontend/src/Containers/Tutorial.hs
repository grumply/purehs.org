module Containers.Tutorial where

import Pure
import Pure.Fetch

import Scope

import Data.Maybe

data T = T Txt deriving (Eq,Ord)

fetcher view = withTut $ \t -> fetch
  Fetcher
    { request = req Scope.getTutorial t
    , cached  = True
    , key     = T t
    , ..
    }
