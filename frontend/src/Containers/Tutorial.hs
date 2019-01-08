module Containers.Tutorial where

import Pure
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

import Data.Maybe
import Data.Proxy
import Debug.Trace
import Control.Concurrent

container :: (TutScope, Caching) => View -> (Maybe Tutorial -> View) -> View
container fallback render = withTut $ \t ->
  let
    lookup :: Maybe (Maybe Tutorial)
    lookup = load t

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getTutorial t (store t)

  in
    asyncAs @Tutorial fetch $
      suspense 1000000 fallback render lookup
