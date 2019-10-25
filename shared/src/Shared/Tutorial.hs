module Shared.Tutorial where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)

import GHC.Generics (Generic)

data TutorialMeta = TutorialMeta
  { number :: {-# UNPACK #-}!Txt
  , slug   :: {-# UNPACK #-}!Txt
  , title  :: {-# UNPACK #-}!Txt -- work around GHCJS toTitle bug
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Tutorial = Tutorial
  { meta    :: {-# UNPACK #-}!TutorialMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

