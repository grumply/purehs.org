{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Page where

import Shared.Types (Description,Excerpt,Slug,Title)

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,ToTxt,FromTxt)

import GHC.Generics (Generic)

import Pure.Data.Txt.Search (Search)

data Page = Page
  { slug :: Slug
  , description :: Description
  } deriving (Generic,ToJSON,FromJSON)

newtype PageContent content = PageContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content
