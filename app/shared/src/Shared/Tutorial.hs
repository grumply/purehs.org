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
  , Markdown
  , Synopsis
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
  , series :: Maybe Series
  , episode :: Maybe Episode
  , slug :: Slug
  , published :: Published
  , edited :: Maybe Edited
  , authors :: Authors
  , editors :: Editors
  , tags :: Tags
  , packages :: Packages
  , synopsis :: Synopsis
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Eq (Tutorial format) where
  (==) = (==) `on` slug

instance Ord (Tutorial format) where
  compare = compare `on` (series &&& episode &&& published)

type TutorialYaml = Tutorial Txt
type TutorialView = Tutorial Markdown

newtype TutorialContent content = TutorialContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content

type TutorialContentMarkdown = TutorialContent Txt
type TutorialContentView = TutorialContent Markdown