{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, AllowAmbiguousTypes #-}
module Components.Searcher (searcher,Msg(..)) where

import Styles.Themes

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm hiding (Class,Data,Type,Pattern)

import Data.Typeable

import Pure.Data.JSON (logJSON)

type Renderer x = (Txt -> IO ()) -> [x] -> View

data Env x = Env [x] (Renderer x)
data Msg = Run | Search Txt
data Model x = Model Txt [x]

searcher :: (Search x, _) => Renderer x -> [x] -> View
searcher f xs = run (App [Run] [Run] [] (Model "" []) update view) (Env xs f)

update :: (Search x) => Msg -> Env x -> Model x -> IO (Model x)
update Run (Env es _) (Model q _) = pure (Model q (containing def q es))
update (Search q) (Env es _) _    = pure (Model q (containing def q es))

view :: Elm Msg => Env x -> Model x -> View
view (Env _ f) (Model _ rs) = f (command . Search) rs
