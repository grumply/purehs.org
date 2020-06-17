module Components.Section where

import Data.Render

import qualified Shared.Types as Types

import Styles.Colors
import Styles.Fonts
import Styles.Responsive
import Styles.Themes

import Pure.Elm.Application hiding (Section)
import qualified Pure.Elm.Application as Elm

import Prelude hiding (max)

data Section = Section View View

instance Render Section where
  render (Section header content) =
    Elm.Section <| Themed @SectionT |>
      [ Header <||>
        [ H2 <||> [ header ] ]
      , content
      ]

data SectionT
instance Theme SectionT where
  theme c = void $
    is c $ do
      apply $ do
        width  =: (100%)
        margin =* [0,auto]

      mediumScreens <%> do
        width =: 520px

      largeScreens <%> do
        width =: 700px

      has (tag Header) $ do
        apply $ do
          text-align =: initial
          width      =: (100%)
          max-width  =: (100%)
          margin     =* [0,auto,15px]

        has (tag H1) .> do
          font-family =: titleFont
          font-size   =: 1.5em
          font-weight =: 400
          color       =: toTxt base

        has (tag H2) .> do
          font-family =: titleFont
          font-size   =: 1.3em
          font-weight =: 400
          color       =: toTxt base

        has (tag H3) .> do
          font-family =: titleFont
          font-size   =: 1.2em
          font-weight =: 400
          color       =: toTxt base

