{-# language DeriveAnyClass #-}
module Shared.Tutorial where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import Data.Function (on)
import GHC.Generics (Generic)

data Meta = Meta
  { number :: {-# UNPACK #-}!Txt
  , slug   :: {-# UNPACK #-}!Txt
  , title  :: {-# UNPACK #-}!Txt -- work around GHCJS toTitle bug
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Tutorial = Tutorial
  { meta    :: {-# UNPACK #-}!Meta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

instance Eq Tutorial where
  (==) = (==) `on` meta

instance Ord Tutorial where
  compare = compare `on` meta