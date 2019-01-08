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

newtype Post_ = Post_ Txt
  deriving (Eq,Ord)

fetcher :: (PostScope, Caching) => View
fetcher = withPost $ \p ->
  let
    lookup :: Maybe (Maybe Post)
    lookup = load (Post_ p)

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getPost p (store (Post_ p))
  in
    asyncAs @Post fetch Null

container :: (PostScope, Caching) => View -> (Maybe Post -> View) -> View
container fallback render =
  withPost (maybe fallback render . load . Post_)
