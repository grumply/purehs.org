module Containers.Doc where

import Pure
import Pure.Fetch

import Scope

import Data.Maybe

newtype D = D (Txt,Txt)
  deriving (Eq,Ord)

fetcher view = withDoc $ \d -> fetch
  Fetcher
    { request = req Scope.getDoc d
    , cached  = True
    , key     = D d
    , ..
    }
