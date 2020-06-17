{-# LANGUAGE AllowAmbiguousTypes #-}
module Components.Searcher (searcher,Search,Renderer,Msg(..)) where

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm

import Control.Arrow
import Data.Typeable

type Renderer x = Txt -> (Txt -> IO ()) -> [x] -> View

data Env x = Env [x] (Renderer x)
data Msg = Run | Search Txt
data Model x = Model Txt [x]

{-# INLINE searcher #-}
searcher :: (Search x, Typeable x) => Renderer x -> [x] -> View
searcher f xs = run (App [Run] [Run] [] (Model "" []) update view) (Env xs f)

{-# INLINE update #-}
update :: (Search x) => Msg -> Env x -> Model x -> IO (Model x)
update Run (Env es _) (Model q _) = pure (Model q (containing def q es))
update (Search q) (Env es _) _ = pure (Model q (containing def q es))

{-# INLINE view #-}
view :: Elm Msg => Env x -> Model x -> View
view (Env _ f) (Model q rs) = f q (command . Search) rs
