module Shared.Package where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import Data.Function (on)
import GHC.Generics (Generic)

data Package = Package
  { package  :: {-# UNPACK #-}!Txt
  , content  :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

instance Eq Package where
  (==) = (==) `on` package

instance Ord Package where
  compare = compare `on` package
