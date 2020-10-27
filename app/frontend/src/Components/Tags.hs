module Components.Tags where

import Data.Render
import Data.Route

import Shared.Types (Tags(..),Tag(..))

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Elm.Application hiding (render,black,brightness,gray,green,lavender)

import GHC.Exts (IsList(..))

instance Render Tags where
  render ts = 
    P <| Themed @TagsT |>
      [ Span <| Cursor "default" |> [ txt t ]
      | Tag t <- toList ts
      ]

instance Render (Tags,Txt -> IO()) where
  render (ts,searcher) =
    P <| Themed @TagsT |>
      [ Span <| OnClick (\_ -> searcher t) . Cursor pointer |> [ txt t ]
      | Tag t <- toList ts
      ]

data TagsT
instance Theme TagsT where
  theme c =
    is c do
      line-height =: 2.5

      has (tag Span) do
        line-height      =: 1.3
        display          =: inline-block
        font-family      =: titleFont
        background-color =: toTxt (faded green)
        border           =* [1px,solid,toTxt (green)]
        color            =: toTxt black
        font-weight      =: 400
        border-radius    =: 7px
        padding          =* [4px,8px]
        margin-right     =: 16px
      
        within @PlaceholderT do
          color       =: transparent
          text-shadow =* [0,0,10px,rgba(0,0,0,0.5)]
          border      =: none

      lastChild do
        margin-right =: 0