module Components.Author where

import Components.Preload
import Data.Render
import Data.Route

import Shared.Types (Name,Authors)

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Elm.Application hiding (render,Title,green,black,lavender,brightness,blue)

import Data.List as List
import GHC.Exts (IsList(..))

instance Render Authors where
  render as =
    Div <| Themed @AuthorsT |>
      List.intersperse ", " 
        [ render (Author a) 
        | a <- toList as 
        ]

newtype Author = Author Name

instance Render Author where
  render (Author nm) =
    Address <| Themed @AuthorT |>
      [ A <| Rel "author" . prelink (AuthorR nm) |>
        [ txt nm ]
      ]

data AuthorsT
instance Theme AuthorsT where
  theme c = void $ is c .> display =: inline

data AuthorT
instance Theme AuthorT where
  theme c =
    is c do
      display     =: inline

      has (tag A) do
        font-family =: titleFont
        font-size   =: 16px
        font-style  =: normal
        color       =: toTxt lavender
         
        hover do
          color =: toTxt green

      within @PlaceholderT do
        pointer-events =: none
        filter_        =: blur(8px)
 
