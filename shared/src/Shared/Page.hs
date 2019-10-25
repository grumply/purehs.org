module Shared.Page where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import Data.Function (on)
import GHC.Generics (Generic)

data Meta = Meta
  { slug :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Page = Page
  { meta :: {-# UNPACK #-}!Meta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

instance Eq Page where
  (==) = (==) `on` meta

instance Ord Page where
  compare = compare `on` meta