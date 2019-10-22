module View.About where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm hiding (content)
import Pure.Theme

import Components.Header
import Components.Titler
import Components.With
import Shared (Page(..),Cache(..))
import Themes
import Types
import Utils

import Control.Monad

about :: Model -> View
about model =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
      [ page model ]
    , titler "Pure - About"
    ]

page :: Model -> View
page model =
  Div <| Theme ArticleT |>
    [ case lookup "about" (pages (cache model)) of
        Just (Done pg) -> success pg
        Just Trying    -> loading
        Just Failed    -> "Not Found."
        Nothing        -> with (publish (LoadPage "about")) loading
    ]

success :: Page -> View
success pg =
  Div <| Theme MarkdownT |>
    (fmap captureLocalRefs (Shared.content pg))

loading :: View
loading =
  Div <| Theme LoadingT |>
    [ "Loading Page" ]
