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
      [ Span <||> [ txt t ]
      | Tag t <- toList ts
      ]

data TagsT
instance Theme TagsT where
  theme c = void $ do
    is c .> do
      line-height =: 2.5
    is c . has (tag Span) $ do
      apply $ do
        line-height      =: 1.3
        display          =: inline-block
        font-family      =: titleFont
        background-color =: toTxt base
        border           =* [1px,solid,toTxt (faded green)]
        color            =: toTxt black
        font-weight      =: 300
        border-radius    =: 7px
        padding          =* [4px,8px]
        margin-right     =: 16px

      is lastChild .>
        margin-right =: 0

    is (subtheme @PlaceholderT) . has c . has (tag Span) .> do
      color       =: transparent
      text-shadow =* [0,0,10px,rgba(0,0,0,0.5)]
      border      =: none
