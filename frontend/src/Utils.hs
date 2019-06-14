{-# LANGUAGE CPP #-}
module Utils where

import Pure.Elm
import Pure.Data.Txt as Txt

import Data.Map as Map

-- quick and dirty solution
captureLocalRefs :: View -> View
captureLocalRefs v =
  let ps = attributes (getFeatures v)
      v' = setChildren (fmap captureLocalRefs (getChildren v)) v
  in case Map.lookup "href" ps of
       Just ref ->
         case Txt.uncons ref of
           Just ('/',_) -> lref ref v'
           _ -> v'
       Nothing -> v'


