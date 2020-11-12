{-# language DeriveAnyClass #-}
module Data.Entity where

import qualified App
import Components.CopyToClipboard
import Data.Route

import Shared.Package as Package
import Shared.Types as Types

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm.Application

import Control.Applicative
import Data.List as List
import Data.Map as Map
import GHC.Generics
import GHC.Exts (IsList(..))

newtype EntityView = EntityView [View]
instance Search EntityView where contains _ _ _ = False

data Entity = Entity EntityType Txt EntityView
  deriving (Generic,Search)

entityType :: Entity -> EntityType
entityType (Entity ety _ _) = ety

data EntityType = DataType | Class__ | Function | Pattern_
  deriving (Generic,Search,Eq,Ord)

entities :: PackageName -> Types.Version -> (Module Rendered,ModuleContent Rendered) -> [Entity]
entities pn v (Module { name = m },ModuleContent (Rendered md)) = 
  snd $ List.foldl extract id (GHC.Exts.toList md) ([],[]) 
  where
    extract :: (([View],[Entity]) -> ([View],[Entity])) 
            -> View 
            -> ([View],[Entity]) -> ([View],[Entity])
    extract continue v@(Children [TextView _ i] H2) (vs,es) = continue $
      if | Txt.isPrefixOf "data"    i -> ([],Entity DataType i (EntityView vs) : es)
         | Txt.isPrefixOf "type"    i -> ([],Entity DataType i (EntityView vs) : es)
         | Txt.isPrefixOf "class"   i -> ([],Entity Class__  i (EntityView vs) : es)
         | Txt.isPrefixOf "pattern" i -> ([],Entity Pattern_ i (EntityView vs) : es)
         | otherwise                  -> ([],Entity Function i (EntityView vs) : es)
    extract continue v (vs,es) = continue (v:vs,es)

linkEntities :: PackageName -> Types.Version -> ModuleName -> [View] -> [View]
linkEntities pn v mn = go
  where
    go [] = []
    go (Children [t@(TextView _ e)] H2 : rest) = H2 <||> [ A <| link (EntityR pn v mn e) |> [ t ] ] : go rest
    go (x : xs) = x : go xs

rebaseEntityLinks :: PackageName -> Types.Version -> ModuleName -> Entity -> Entity
rebaseEntityLinks pn v (ModuleName mn) (Entity ety nm (EntityView vs)) = Entity ety nm (EntityView $ fmap go vs)
  where
    go v =
      let as = attributes (getFeatures v)
          ps = properties (getFeatures v)
          v' = setChildren (fmap go (getChildren v)) v
          ipo = Txt.isPrefixOf
      in case Map.lookup "href" ps <|> Map.lookup "href" as of
           Just r 
            |          mn  `ipo` r -> Attribute "href" ("../" <> r) v'
            | ("./" <> mn) `ipo` r -> Attribute "href" ("."   <> r) v'
           _ -> v'

