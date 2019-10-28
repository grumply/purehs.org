{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable #-}
module View.Docs.Searcher (searcher) where

import Themes

import Shared

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm hiding (Class,Data,Type,Pattern)
import Pure.Router (lref)

import Data.Set as Set

import Data.Data (Data,toConstr)
import Data.Function (on)
import Data.List as List
import GHC.Generics (Generic)

data Entity
  = DataType Txt
  | Class Txt
  | Function Txt
  | Pattern Txt
  deriving (Generic,Search,Data)

searcher :: Txt -> Txt -> [[View]] -> View
searcher p v ms = run (App [] [] [] Nothing update view) (p,v,fmap entries ms)
  where
    entries :: [View] -> (Txt,[Entity])
    entries (Children [ TextView _ m ] H2 : es) = (m,List.foldr extract [] es)
      where
        extract :: View -> [Entity] -> [Entity]
        extract (Children [TextView _ i] H3) es =
          if | Txt.isPrefixOf "data"  i -> (DataType i) : es
             | Txt.isPrefixOf "type"  i -> (DataType i) : es
             | Txt.isPrefixOf "class" i -> (Class i) : es
             | Txt.isPrefixOf "pattern" i -> (Pattern i) : es
             | otherwise                -> (Function i) : es
        extract _ es = es

update :: Txt -> (Txt,Txt,[(Txt,[Entity])]) -> Maybe [(Txt,[Entity])] -> IO (Maybe [(Txt,[Entity])])
update msg (p,v,es) _
  | Txt.null msg = pure Nothing
  | otherwise    = pure (Just $ fmap (fmap (containing def msg)) es)

view :: Elm Txt => (Txt,Txt,[(Txt,[Entity])]) -> Maybe [(Txt,[Entity])] -> View
view (p,v,es) mdl =
  Div <| Theme SearcherColumnT |>
    [ Div <| Theme SearcherT |>
      [ Input <| OnInput (withInput command) . Placeholder "Search"
      , case mdl of
          Nothing -> results p v (fmap (fmap (const [])) es)
          Just rs -> results p v rs
      ]
    ]

results :: Txt -> Txt -> [(Txt,[Entity])] -> View
results p v rs
    | List.all (List.null . snd) rs =
      Div <||>
        [ Div <| Theme EntryT . Theme ModuleT |>
          [ A <| lref ("/doc/" <> p <> "/" <> v <> "/" <> m) |>
            [ fromTxt m ]
          ]
        | m <- fmap fst rs
        ]
    | otherwise =
      Div <||>
        [ if List.null es then
            Null
          else
            Div <||>
              [ Div <| Theme EntryT . Theme ModuleT |>
                [ A <| lref ("/doc/" <> p <> "/" <> v <> "/" <> m) |>
                  [ fromTxt m ]
                ]
              , Div <||> fmap (entity p v m) gs
              ]
        | (m,es) <- rs
        , let gs = List.concat $ List.groupBy ((==) `on` toConstr) es
        ]

entity :: Txt -> Txt -> Txt -> Entity -> View
entity p v m (DataType d) =
  Div <| Theme EntryT . Theme DataT |>
    [ A <| lref ("/doc/" <> p <> "/" <> v <> "/" <> m <> "/" <> d) |>
      [ fromTxt d ]
    ]

entity p v m (Class c) =
  Div <| Theme EntryT . Theme ClassT |>
    [ A <| lref ("/doc/" <> p <> "/" <> v <> "/" <> m <> "/" <> c) |>
      [ fromTxt c ]
    ]

entity p v m (Function f) =
  Div <| Theme EntryT . Theme FunctionT |>
    [ A <| lref ("/doc/" <> p <> "/" <> v <> "/" <> m <> "/" <> f) |>
      [ fromTxt f ]
    ]

entity p v m (Pattern pt) =
  Div <| Theme EntryT . Theme PatternT |>
    [ A <| lref ("/doc/" <> p <> "/" <> v <> "/" <> m <> "/" <> pt) |>
      [ fromTxt pt ]
    ]

data SearcherColumnT = SearcherColumnT
instance Themeable SearcherColumnT where
  theme c _ = void $
    is c $ do
      id .> do
        minWidth =: pxs 260
        overflowY =: scroll

      atMedia "(min-width: 48em)" .> do
        marginTop =: pxs 64

data EntryT = EntryT
instance Themeable EntryT where
  theme c _ = void $
    is c . child "a" $ do
      id .> do
        marginLeft =: pxs 16
        fontFamily =: "source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
        fontWeight =: int 800
        textDecoration =: none
        color =: darkGray
      is ":hover" .> do
        textDecoration =: underline
      is ":visited" .> do
        textDecoration =: none
        color =: darkGray

entry c clr = void $ do
  is c . nexts c .> marginTop =: zero
  is c .> marginTop =: pxs 8
  is c . child "a" $ do
    id .> color =: clr
    is ":visited" .> color =: clr

data SearcherT = SearcherT
instance Themeable SearcherT where
  theme c _ = void $
    is c $ do

      atMedia "(min-width: 48em)" .> do
        important $ minHeight =: zero
        position =: "sticky"
        top =: pxs 32
        bottom =: pxs 32
        overflowY =: scroll
        marginLeft =: pxs 16
        paddingLeft =: pxs 16
        paddingRight =: pxs 16
        borderLeft =: pxs 1 <<>> solid <<>> lightGray

      child "input" .> do
        width =: per 100
        lineHeight =: dec 1.2
        fontSize =: pxs 18

data ModuleT = ModuleT
instance Themeable ModuleT where
  theme c _ = do
    is c . child "a" .> important (marginLeft =: zero)
    entry c darkGray

data DataT = DataT
instance Themeable DataT where
  theme c _ = entry c purple_

data ClassT = ClassT
instance Themeable ClassT where
  theme c _ = entry c purple_

data FunctionT = FunctionT
instance Themeable FunctionT where
  theme c _ = entry c blue_

data PatternT = PatternT
instance Themeable PatternT where
  theme c _ = entry c green_