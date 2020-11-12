module Components.Tags where

import Shared.Types (Tags(..),Tag(..))

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Elm.Application hiding (render,black,brightness,gray,green,lavender)

tags :: Tags -> View
tags (Tags ts) = 
  P <| Themed @Tags |>
    [ Span <| Cursor "default" |> [ txt t ]
    | Tag t <- ts
    ]

data TagLinks = TagLinks Tags (Txt -> IO ())

searchableTags :: Tags -> (Txt -> IO ()) -> View
searchableTags (Tags ts) searcher =
  P <| Themed @Tags |>
    [ Span <| OnClick (\_ -> searcher t) . Cursor pointer |> [ txt t ]
    | Tag t <- ts
    ]

instance Theme Tags where
  theme c =
    is c do
      has (tag Span) do
        line-height      =: 1.1
        display          =: inline-block
        font-family      =: titleFont
        background-color =: toTxt (faded green)
        border           =* [1px,solid,toTxt (green)]
        color            =: toTxt black
        font-weight      =: 400
        border-radius    =: 7px
        padding          =* [4px,8px]
        margin-right     =: 16px

      lastChild do
        margin-right =: 0

      within @Divided do
        margin =* [15px,0px,20px,10px]