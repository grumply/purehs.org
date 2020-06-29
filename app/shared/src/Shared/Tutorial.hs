{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Tutorial where

import Shared.Types
  ( Title
  , Subtitle
  , Series
  , Episode
  , Slug
  , Published
  , Edited
  , Authors
  , Editors
  , Tags
  , Excerpt
  , Packages
  , Description
  , Short
  )

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,ToTxt,FromTxt)

import Control.Arrow ((&&&))
import Data.Ord (compare)
import Data.Function (on)
import GHC.Generics (Generic)

import Pure.Data.Txt.Search (Search)

data Tutorial format = Tutorial
  { title :: Title
  , subtitle :: Maybe Subtitle
  , slug :: Slug
  , episode :: Maybe Episode
  , series :: Maybe Series
  , published :: Published
  , authors :: Authors
  , tags :: Tags
  , packages :: Packages
  , short :: Short
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Eq (Tutorial format) where
  (==) = (==) `on` slug

instance Ord (Tutorial format) where
  compare = compare `on` (series &&& episode &&& published)

newtype TutorialContent content = TutorialContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content
