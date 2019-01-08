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

newtype Tut_ = Tut_ Txt
  deriving (Eq,Ord)

fetcher :: (TutScope, Caching) => View
fetcher = withTut $ \t ->
  let
    lookup :: Maybe (Maybe Tutorial)
    lookup = load (Tut_ t)

    fetch | isJust lookup = return ()
          | otherwise = req Scope.getTutorial t (store (Tut_ t))

  in
    asyncAs @Tutorial fetch Null

container :: (TutScope, Caching) => View -> (Maybe Tutorial -> View) -> View
container fallback render =
  withTut (maybe fallback render . load . Tut_)

