module Components.Time where

import Data.Render
import Data.Route

import Shared.Types (Published(..))

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Elm.Application hiding (render,Title,brightness,gray)

import Data.List as List
import GHC.Exts (IsList(..))

instance Render Time where
  render t = 
    SimpleHTML "time" <| Themed @TimeT . DateTime (toDateTime t) |>
      [ " ", formatTime "%B %e, %Y " t ]

instance Render Published where
  render (Published t) = render t

data TimeT
instance Theme TimeT where
  theme c = do
    is c do
      font-family =: titleFont
      font-size   =: 16px
      display     =: inline
      color       =: toTxt gray

      within @PlaceholderT do
        pointer-events =: none
        filter_        =: blur(8px)