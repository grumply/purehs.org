module View.About where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm hiding (content)
import Pure.Theme

import Colors
import Components.Header
import Components.Titler
import Shared (Page(..),Cache(..))
import Themes
import Types

import Control.Monad

about :: Model -> View
about model =
  Div <| Theme PageT . Theme AboutT |>
    [ header (route model) False
    , page model
    , titler "Pure - About"
    ]

page :: Model -> View
page model =
  case lookup "about" (pages (cache model)) of
    Just (Done pg) -> success pg
    _              -> loading

success :: Page -> View
success pg = 
  Div <| Theme ContentT . Theme MarkdownT |> 
    (Shared.content pg)

loading :: View
loading = 
  Div <| Theme LoadingT |>
    [ "Loading Page" ]

data AboutT = AboutT
instance Themeable AboutT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $ do
    is c $ do
      headerOffset

      apply $ do
        maxWidth   =: pxs 1200
        margin     =: auto
        padding    =: ems 1

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()


