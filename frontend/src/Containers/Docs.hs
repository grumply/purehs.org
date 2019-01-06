module Containers.Docs where

import Pure
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

container :: View -> (Maybe [Doc] -> View) -> View
container fallback render = caching $ async fetch $ suspense 1000000 fallback render lookup
  where
    proxy  = Proxy @[Doc]
    fetch  = req Scope.getDocs () (store proxy . Just)
    lookup = load proxy
