module Containers.Doc where

import Pure hiding (Doc)
import Pure.Async
import Pure.Cache
import Pure.Suspense

import Scope

import Data.Maybe
import Data.Proxy
import Debug.Trace
import Control.Concurrent

container :: (DocScope, Caching) => View -> (Maybe Doc -> View) -> View
container fallback render = withDoc $ \d ->
  let
    lookup :: Maybe (Maybe Doc)
    lookup = load d

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getDoc d (store d)

  in
    asyncAs @Doc fetch $
      suspense 1000000 fallback render lookup
