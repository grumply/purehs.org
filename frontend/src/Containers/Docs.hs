module Containers.Docs where

import Pure
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

import Data.Maybe
import Data.Proxy
import Control.Concurrent

container :: Caching => View -> ([DocMeta] -> View) -> View
container fallback render =
  let
    proxy :: Proxy [DocMeta]
    proxy = Proxy

    lookup :: Maybe [DocMeta]
    lookup = load proxy

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getDocMetas () (store proxy)

  in
    asyncAs @DocMeta fetch $
      suspense 1000000 fallback render lookup
