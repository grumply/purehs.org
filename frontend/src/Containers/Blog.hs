module Containers.Blog where

import Pure
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

import Data.Maybe
import Data.Proxy
import Control.Concurrent

fetcher :: Caching => View
fetcher =
  let
    proxy :: Proxy [PostMeta]
    proxy = Proxy

    lookup :: Maybe [PostMeta]
    lookup = load proxy

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getPostMetas () (store proxy)
  in
    asyncAs @PostMeta fetch Null

container :: Caching => View -> ([PostMeta] -> View) -> View
container fallback render =
  let
    proxy :: Proxy [PostMeta]
    proxy = Proxy
  in
    maybe fallback render (load proxy)
