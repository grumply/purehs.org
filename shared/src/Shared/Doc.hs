module Shared.Doc where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import Data.Function (on)
import GHC.Generics (Generic)

data Meta = Meta
  { package :: {-# UNPACK #-}!Txt
  , version :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Doc = Doc
  { meta    :: {-# UNPACK #-}!Meta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

instance Eq Doc where
  (==) = (==) `on` meta

instance Ord Doc where
  compare = compare `on` meta