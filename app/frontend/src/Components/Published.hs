module Components.Published where

import Shared.Types ( Published(..) )

import Styles.Colors ( gray )
import Styles.Fonts ( titleFont )

import Pure.Elm.Application hiding (gray)

published :: Published -> View
published (Published t) =
    SimpleHTML "time" <| Themed @Time . DateTime (toDateTime t) |>
      [ " ", formatTime "%B %e, %Y " t ]

instance Theme Time where
  theme c = do
    is c do
      font-family =: titleFont
      font-size   =: 16px
      display     =: inline
      color       =: toTxt gray
