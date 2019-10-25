module Shared.Package where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import Data.Function (on)
import GHC.Generics (Generic)

data Meta = Meta
  { package :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Package = Package
  { meta  :: {-# UNPACK #-}!Meta
  , content  :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

instance Eq Package where
  (==) = (==) `on` meta

instance Ord Package where
  compare = compare `on` meta
