{-# language DeriveAnyClass #-}
module Data.Entity where

import qualified App
import Components.Article
import Components.CopyToClipboard
import Data.Route

import Shared.Package as Package
import Shared.Types as Types

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm.Application

import Data.List as List
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

entities :: PackageName -> Types.Version -> (ModuleView,ModuleContentView) -> [Entity]
entities pn v (Module { name = m },ModuleContent (Markdown md)) = 
  snd $ List.foldl extract id (toList md) ([],[]) 
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

linkEntities pn v mn = go
  where
    go [] = []
    go (Children [t@(TextView _ e)] H2 : rest) = H2 <||> [ A <| link (EntityR pn v mn e) |> [ t ] ] : go rest
    go (x : xs) = x : go xs

