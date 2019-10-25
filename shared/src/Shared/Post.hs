module Shared.Post where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import GHC.Generics (Generic)

data PostMeta = PostMeta
  { year  :: {-# UNPACK #-}!Txt
  , month :: {-# UNPACK #-}!Txt
  , day   :: {-# UNPACK #-}!Txt
  , slug  :: {-# UNPACK #-}!Txt
  , title :: {-# UNPACK #-}!Txt -- work around GHCJS toTitle bug
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Post = Post
  { meta    :: {-# UNPACK #-}!PostMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)
