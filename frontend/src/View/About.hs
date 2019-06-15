module View.About where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm hiding (content)
import Pure.Theme

import Components.Header
import Components.Titler
import Shared (Page(..),Cache(..))
import Themes
import Types

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
        _              -> loading
    ]

success :: Page -> View
success pg = 
  Div <| Theme MarkdownT |> 
    (Shared.content pg)

loading :: View
loading = 
  Div <| Theme LoadingT |>
    [ "Loading Page" ]
