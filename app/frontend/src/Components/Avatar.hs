module Components.Avatar (avatar, avatars, Avatars) where

import Components.Preload ( prelink )
import Data.Route

import Shared.Types ( Name(Name) )

import Styles.Colors ( Color(alpha), green, black )
import Styles.Themes

import Pure.Data.Txt as Txt ( ToTxt(toTxt), replace )
import Pure.Elm.Application hiding (render,Title,green,black,alpha,brightness)

import Prelude hiding (reverse)

avatar :: Name -> View
avatar nm0 = let nm = Txt.replace " " "_" nm0 in
  A <| prelink (AuthorRoute (AuthorR nm0)) |>
    [ Img <| Src ("/static/avatars/" <> toTxt nm <> ".jpg") . Alt (toTxt nm0)
    ]

avatars :: [Name] -> View
avatars as = 
    Div <| Themed @Avatars |>
      [ avatar a
      | a <- as
      ]

data Avatars
instance Theme Avatars where
  theme c =
    is c do
      width           =: (100%)
      position        =: absolute
      top             =: (-35)px
      left            =: 0
      display         =: flex
      flex-direction  =: row-reverse
      justify-content =: center
      text-align      =: center
  
      has (tag A) do
        display      =: flex
        transition   =* [transform,100ms]
        margin-right =: (-35)px
        margin-left  =: (-35)px
        z-index      =: 1

        hover do
          z-index    =: 2
          transition =* [transform,200ms,ease]
          transform  =: scale(1.3)

        active do
          z-index    =: 2
          transition =* [transform,400ms,ease]
          transform  =: scale(0.7)

        has (tag Img) do
          pointer-events =: none
          border-radius  =: (50%)
          width          =: 130px
          height         =: 130px
          border         =* [5px,solid,toTxt green]
          box-shadow     =: customBoxShadow 0 15 30 (-5) (toTxt black { alpha = 0.65 })
 
      within @Subarticles do
        display =: none     