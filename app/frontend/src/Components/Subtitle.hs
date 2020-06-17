module Components.Subtitle where

import Data.Render

import qualified Shared.Types as Types

import Styles.Colors
import Styles.Fonts
import Styles.Themes

import Pure.Elm

data Subtitle = Subtitle (Maybe Types.Subtitle)

instance Render Subtitle where
  render (Subtitle subtitle)
    | Just sub <- subtitle = 
      H2 <| Themed @SubtitleT |> 
        [ txt sub ]

    | otherwise = 
      Null

data SubtitleT
instance Theme SubtitleT where
  theme c = void $ do
    is (subtheme @PlaceholderT) . has c .> do
      pointer-events =: none
      filter_        =: blur(12px)

    is c .> do
      font-family =: titleFont
      font-size   =: 1.3em
      font-weight =: 400
      color       =: toTxt base
