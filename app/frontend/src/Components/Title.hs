module Components.Title where

import qualified App (App)
import Data.Route ( Route )
import Components.Preload ( prelink )

import Styles.Colors ( green, black )
import Styles.Fonts ( titleFont )
import Styles.Themes ( Article, Listing )

import Pure.Elm hiding (render,Title,green,black)

data Title = Title Route Txt
  
title :: App.App => Route -> Txt -> View
title rt t =
  H1 <| Themed @TitleT |> 
    [ A <| prelink rt |> 
      [ txt t ]
    ]

data TitleT
instance Theme TitleT where
  theme c =
    is c do
      margin-bottom =: 10px

      has (tag A) do
        font-family =: titleFont
        font-size   =: 1.45em
        font-weight =: 400
        color       =: toTxt black
        
        hover do
          color =: toTxt green

      within @Listing do
        display   =: inline-block
        font-size =: 1em
        
      within @Article do
        display =: block

