{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, AllowAmbiguousTypes #-}
module Components.PackageSearch (packageSearch) where

import Components.Searcher
import Data.Route
import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm hiding (Class,Data,Type,Pattern)
import Pure.Elm.Application (link)

import Data.Data (Data)
import Data.Function (on)
import Data.List as List
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Ord
import Data.Typeable
import GHC.Generics (Generic)

packageSearch :: Txt -> Txt -> Maybe Txt -> [[View]] -> View
packageSearch ctx p v md = searcher (packageSearchView ctx ms) es
  where
    ms = mapMaybe (module_ p v) md
    es = List.concatMap (entries p v) md

data Entity = Entity EntityType Txt (Maybe Txt) Txt Txt
  deriving (Generic,Search,Data)

entityType :: Entity -> EntityType
entityType (Entity ety _ _ _ _) = ety

entityModule :: Entity -> Txt
entityModule (Entity _ _ _ m _) = m

data EntityType = DataType | Class | Function | Pattern
  deriving (Generic,Search,Data,Eq,Ord)

module_ :: Txt -> Maybe Txt -> [View] -> Maybe (Txt,Maybe Txt,Txt)
module_ p v (Children [ TextView _ m ] H2 : _) = Just (p,v,m)
module_ _ _ _ = Nothing

entries :: Txt -> Maybe Txt -> [View] -> [Entity]
entries p v (Children [ TextView _ m ] H2 : es) = List.foldr (extract m) [] es
  where
    extract :: Txt -> View -> [Entity] -> [Entity]
    extract m (Children [TextView _ i] H3) es =
      if | Txt.isPrefixOf "data"    i -> Entity DataType p v m i : es
         | Txt.isPrefixOf "type"    i -> Entity DataType p v m i : es
         | Txt.isPrefixOf "class"   i -> Entity Class    p v m i : es
         | Txt.isPrefixOf "pattern" i -> Entity Pattern  p v m i : es
         | otherwise                  -> Entity Function p v m i : es
    extract _ _ es = es
entries _ _ _ = []

packageSearchView :: Txt -> [(Txt,Maybe Txt,Txt)] -> (Txt -> IO ()) -> [Entity] -> View
packageSearchView ctx ms search rs = 
  Div <||>
    ( Input <| Theme SearcherT . OnInput (withInput search) . Placeholder ("Search " <> ctx) 
    : modules rs
    )
  where
    modules :: [Entity] -> [View]
    modules [] = fmap moduleHeader ms 
    modules es = fmap results 
               $ fmap (List.sortBy (comparing entityType))
               $ List.groupBy ((==) `on` entityModule)
               $ List.sortBy (comparing entityModule) es

results :: [Entity] -> View
results es@(Entity _ p v m _ : _) = 
  Div <||> 
    ( moduleHeader (p,v,m)
    : fmap entity es
    )

moduleHeader :: (Txt,Maybe Txt,Txt) -> View
moduleHeader (p,v,m) =
  H2 <| Theme ModuleT |>
    [ A <| link (ModuleR p v m) |> 
      [ text m ]
    ]

entity :: Entity -> View
entity (Entity ety p v m e) = 
  Div <| toTheme ety |>
    [ A <| Theme EntryT . link (EntityR p v m e) |>
      [ text e ]
    ]
  where
    toTheme Pattern  = Theme PatternT
    toTheme Function = Theme FunctionT
    toTheme Class    = Theme ClassT
    toTheme DataType = Theme DataT

data EntryT = EntryT
instance Themeable EntryT where
  theme c _ = void $ is c $ do
    id .> do
      marginLeft =: pxs 16
      fontFamily =: defaultMonoFont
      fontWeight =: int 800
      textDecoration =: none
      color =: darkGray
    is ":hover" .> do
      textDecoration =: underline
    is ":visited" .> do
      textDecoration =: none
      color =: darkGray

data SearcherT = SearcherT
instance Themeable SearcherT where
  theme c _ = void $ is c .> do
    fontSize =: pxs 24
    width =: per 100
    marginTop =: pxs 24
    marginBottom =: pxs 24
    borderRadius =: pxs 8
    border =: pxs 1 <<>> solid <<>> "#eeeeee"
    outline =: none
    lineHeight =: dec 1.2
    padding =: pxs 10

data ModuleT = ModuleT
instance Themeable ModuleT where
  theme c _ = void $
    is c $ do
      id .>
        marginTop =: pxs 8

      child "a" $ do
        id .> do
          important (marginLeft =: zero)
          color =: darkLavender
          textDecoration =: none
        is ":visited" .>
          color =: darkLavender

entry :: Txt -> Txt -> CSS ()
entry c clr = void $ do
  is c . nexts c .> marginTop =: zero
  is c .> marginTop =: pxs 8
  is c . child "a" $ do
    id .> color =: clr
    is ":visited" .> color =: clr

data DataT = DataT
instance Themeable DataT where
  theme c _ = entry c cyan_

data ClassT = ClassT
instance Themeable ClassT where
  theme c _ = entry c purple_

data FunctionT = FunctionT
instance Themeable FunctionT where
  theme c _ = entry c blue_

data PatternT = PatternT
instance Themeable PatternT where
  theme c _ = entry c green_
