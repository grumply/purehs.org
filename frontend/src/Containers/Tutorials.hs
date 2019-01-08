module Containers.Tutorials where

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
    proxy :: Proxy [TutorialMeta]
    proxy = Proxy

    lookup :: Maybe [TutorialMeta]
    lookup = load proxy

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getTutorialMetas () (store proxy)
  in
    asyncAs @TutorialMeta fetch Null

container :: Caching => View -> ([TutorialMeta] -> View) -> View
container fallback render =
  let
    proxy :: Proxy [TutorialMeta]
    proxy = Proxy
  in
    maybe fallback render (load proxy)
