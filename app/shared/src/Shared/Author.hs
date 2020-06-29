{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Author where

import Shared.Types
  ( Name
  , GitHubName
  , TwitterHandle
  , Email
  , Company
  , Excerpt
  , Description
  )

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,ToTxt,FromTxt)

import Data.Ord (compare)
import Data.Function (on)
import GHC.Generics (Generic)

import Pure.Data.Txt.Search (Search)

data Author format = Author
  { name :: Name
  , github :: Maybe GitHubName
  , twitter :: Maybe TwitterHandle
  , email :: Maybe Email
  , company :: Maybe Company
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Ord (Author view) where
  compare = compare `on` name 

instance Eq (Author view) where
  (==) = (==) `on` name

newtype AuthorContent content = AuthorContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content
