{-# LANGUAGE CPP #-}
module Utils where

import Pure.Elm
import Pure.Router (lref)
import Pure.Data.Txt as Txt

import Control.Applicative
import Data.Map as Map

-- quick and dirty solution
captureLocalRefs :: View -> View
captureLocalRefs v =
  let as = attributes (getFeatures v)
      ps = properties (getFeatures v) 
      v' = setChildren (fmap captureLocalRefs (getChildren v)) v
  in case Map.lookup "href" ps <|> Map.lookup "href" as of
       Just ref ->
         case Txt.uncons ref of
           Just ('/',_) -> lref ref v'
           _ -> v'
       Nothing -> v'

