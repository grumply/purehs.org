{-# LANGUAGE NoMonomorphismRestriction #-}
module Containers.Examples where

import Pure
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

import Data.Maybe
import Data.Proxy
import Control.Concurrent

container :: Caching => View -> ([Example] -> View) -> View
container fallback render =
  let
    proxy :: Proxy [Example]
    proxy = Proxy

    lookup :: Maybe [Example]
    lookup = load proxy

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getExamples () (store proxy)

  in
    asyncAs @Example fetch $
      suspense 1000000 fallback render lookup
