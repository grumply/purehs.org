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

newtype Doc_ = Doc_ (Txt,Txt)
  deriving (Eq,Ord)

fetcher :: (DocScope, Caching) => View
fetcher = withDoc $ \d ->
  let
    lookup :: Maybe (Maybe Doc)
    lookup = load (Doc_ d)

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getDoc d (store (Doc_ d))
  in
    asyncAs @Doc fetch Null

container :: (DocScope, Caching) => View -> (Maybe Doc -> View) -> View
container fallback render =
  withDoc (maybe fallback render . load . Doc_)
