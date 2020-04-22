{-# language DeriveAnyClass #-}
module Shared.Post where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import Data.Function (on)
import GHC.Generics (Generic)

data Meta = Meta
  { year  :: {-# UNPACK #-}!Txt
  , month :: {-# UNPACK #-}!Txt
  , day   :: {-# UNPACK #-}!Txt
  , slug  :: {-# UNPACK #-}!Txt
  , title :: {-# UNPACK #-}!Txt -- work around GHCJS toTitle bug
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Post = Post
  { meta    :: {-# UNPACK #-}!Meta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

instance Eq Post where
  (==) = (==) `on` meta

instance Ord Post where
  compare = compare `on` meta