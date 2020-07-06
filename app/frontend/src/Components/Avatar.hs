module Components.Avatar where

import Components.Preload
import Data.Render
import Data.Route

import Shared.Types (Name(..))

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Data.Txt as Txt hiding (center,index,reverse)
import Pure.Elm.Application hiding (render,Title,green,black,alpha,brightness)

import Data.List as List hiding (reverse)
import GHC.Exts (IsList(..))

import Prelude hiding (reverse)

newtype Avatar = Avatar Name

instance Render Avatar where
  render (Avatar nm0) = let nm = Txt.replace " " "_" nm0 in
    A <| prelink (AuthorR nm0) |>
      [ Img <| Src ("/static/avatars/" <> toTxt nm <> ".jpg") . Alt (toTxt nm0)
      ]

newtype Avatars = Avatars [Name]

instance Render Avatars where
  render (Avatars as) = 
    Div <| Themed @AvatarsT |>
      [ render (Avatar a) 
      | a <- as
      ]

data AvatarsT
instance Theme AvatarsT where
  theme c = void $ do
    is (subtheme @PlaceholderT) . has c $ do
      apply $ 
        top =: (-165)px

      has (tag Img) .> 
        filter_ =: blur(10px)

    is c $ do
      apply $ do
        width           =: (100%)
        position        =: absolute
        top             =: (-35)px
        left            =: 0
        display         =: flex
        flex-direction  =: row-reverse
        justify-content =: center
        text-align      =: center
    
      has (tag A) $ do
        apply $ do
          display      =: flex
          transition   =* [transform,100ms]
          margin-right =: (-35)px
          margin-left  =: (-35)px
          z-index      =: 1

        is hover .> do
          z-index    =: 2
          transition =* [transform,200ms,ease]
          transform  =: scale(1.3)

        is active .> do
          z-index    =: 2
          transition =* [transform,400ms,ease]
          transform  =: scale(0.7)

        has (tag Img) .> do
          pointer-events =: none
          border-radius  =: (50%)
          width          =: 130px
          height         =: 130px
          border         =* [5px,solid,toTxt green]
          box-shadow     =: customBoxShadow 0 15 30 (-5) (toTxt black { alpha = 0.65 })

