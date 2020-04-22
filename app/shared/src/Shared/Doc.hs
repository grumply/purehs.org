{-# language DeriveAnyClass #-}
module Shared.Doc where

import Shared.Utils (breakMany)

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()
import Pure.Data.Txt (Txt)
import Pure.Data.View (View)
import Pure.Elm (pattern H2)

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

-- | Break a Doc into an introduction, and a list of modules broken up by H2s.
breakDoc :: Doc -> ([View],[[View]])
breakDoc d =
  let
    h2 H2 = True
    h2 _ = False
   in
    breakMany h2 (content d)

