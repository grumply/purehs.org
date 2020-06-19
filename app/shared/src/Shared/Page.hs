{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Page where

import Shared.Types (Description,Excerpt,Slug,Title,Synopsis)

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,ToTxt,FromTxt)

import GHC.Generics (Generic)

import Pure.Data.Txt.Search (Search)

data Page content = Page
  { slug :: Slug
  , title :: Title
  , synopsis :: Synopsis
  , description :: Description
  , excerpt :: Excerpt content
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

newtype PageContent content = PageContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content
