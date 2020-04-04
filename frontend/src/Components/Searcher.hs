{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable #-}
module Components.Searcher (searcher,Msg(..)) where

import Styles.Themes

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm hiding (Class,Data,Type,Pattern)

data Env x = Env [x] ((Txt -> IO ()) -> Maybe [x] -> View)
data Msg = Receive | Search Txt
data Model x = Model Txt (Maybe [x])

searcher :: (Search x, _) => ((Txt -> IO ()) ->  Maybe [x] -> View) -> [x] -> View
searcher f xs = run (App [] [Receive] [] (Model "" Nothing) update view) (Env xs f)

update :: Search x => Msg -> Env x -> Model x -> IO (Model x)
update (Search q) (Env es _) _
  | Txt.null q = pure (Model q Nothing)
  | otherwise  = do
    let results = containing def q es
    pure (Model q $ Just results)
update Receive (Env es _) (Model q _) = do
  let results = containing def q es
  pure (Model q $ Just results)

view :: Elm Msg => Env x -> Model x -> View
view (Env _ f) (Model _ rs) = f (command . Search) rs
