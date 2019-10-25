module Shared.Page where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import GHC.Generics (Generic)

data PageMeta = PageMeta
  { slug :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Page = Page
  { meta :: {-# UNPACK #-}!PageMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

