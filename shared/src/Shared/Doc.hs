module Shared.Doc where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import GHC.Generics (Generic)

data DocMeta = DocMeta
  { package :: {-# UNPACK #-}!Txt
  , version :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Doc = Doc
  { meta    :: {-# UNPACK #-}!DocMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)
