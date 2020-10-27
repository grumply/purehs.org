module Components.Title where

import Components.Route
import Data.Render
import Data.Route

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Elm hiding (render,Title,green,black)

data Title = Title Route Txt

instance Render Title where
  render (Title rt t) = 
    H1 <| Themed @TitleT |> 
      [ render rt <||> 
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

      within @PlaceholderT do
        pointer-events =: none
        filter_        =: blur(16px)
