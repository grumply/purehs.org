module Components.Subtitle where

import Shared.Types as Types ( Subtitle )

import Styles.Colors ( base )
import Styles.Fonts ( titleFont )

import Pure.Elm

subtitle :: Maybe Subtitle -> View
subtitle = maybe Null sub
  where
    sub s = H2 <| Themed @SubtitleT |> [ txt s ]

data SubtitleT
instance Theme SubtitleT where
  theme c =
    is c do
      font-family =: titleFont
      font-size   =: 1.3em
      font-weight =: 400
      color       =: toTxt base
