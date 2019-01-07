{-# LANGUAGE NoMonomorphismRestriction #-}
module Containers.Post where

import Pure
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

import Data.Maybe
import Data.Proxy
import Debug.Trace
import Control.Concurrent

container :: (PostScope, Caching) => View -> (Maybe Post -> View) -> View
container fallback render = withPost $ \p ->
  let
    lookup :: Maybe (Maybe Post)
    lookup = load p

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getPost p (store p)

  in
    asyncAs @Post fetch $
      suspense 1000000 fallback render lookup
