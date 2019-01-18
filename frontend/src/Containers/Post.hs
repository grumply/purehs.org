module Containers.Post where

import Pure hiding (P)
import Pure.Fetch

import Scope

import Data.Maybe

newtype P = P Txt
  deriving (Eq,Ord)

fetcher view = withPost $ \p -> fetch
  Fetcher
    { request = req Scope.getPost p
    , cached  = True
    , key     = P p
    , ..
    }
