{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Author where

import Shared.Types
  ( Name
  , GitHubName
  , TwitterHandle
  , Email
  , Company
  , Excerpt
  , Markdown
  , Description
  , Synopsis
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
  , synopsis :: Synopsis
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Ord (Author view) where
  compare = compare `on` name 

instance Eq (Author view) where
  (==) = (==) `on` name

type AuthorYaml = Author Txt
type AuthorView = Author Markdown

newtype AuthorContent content = AuthorContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content

type AuthorContentMarkdown = AuthorContent Txt
type AuthorContentView = AuthorContent Markdown