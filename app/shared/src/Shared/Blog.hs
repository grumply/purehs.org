{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Blog where

import Shared.Types
  ( Title
  , Subtitle
  , Episode
  , Slug
  , Published
  , Edited
  , Authors
  , Editors
  , Tags
  , Excerpt
  , Description
  , Synopsis
  )

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,ToTxt,FromTxt)

import Control.Arrow ((&&&))
import Data.Function (on)
import GHC.Generics (Generic)

import Pure.Data.Txt.Search (Search)

data Post format = Post
  { title :: Title
  , subtitle :: Maybe Subtitle
  , slug :: Slug
  , episode :: Maybe Episode
  , published :: Published
  , edited :: Maybe Edited
  , authors :: Authors
  , editors :: Editors
  , tags :: Tags
  , synopsis :: Synopsis
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Ord (Post format) where
  compare = compare `on` (published &&& slug)

instance Eq (Post format) where
  (==) = (==) `on` (published &&& slug)

newtype PostContent content = PostContent content
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON)
    via content
