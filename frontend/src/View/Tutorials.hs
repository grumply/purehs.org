module View.Tutorials where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Theme

import qualified Shared (Cache(..),Tutorial(..),TutorialMeta(..))
import Themes
import Types
import Utils

import Components.Header (header)
import Components.Icons  (logo)
import Components.Titler (titler)

import Control.Monad

tutorials :: Model -> View
tutorials model =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
      [ case route model of
          TutsR Nothing  -> listing model
          TutsR (Just t) -> tutorial t model
      ]
    , titler $
        case route model of
          TutsR (Just t) -> "Pure - " <> t
          _ -> "Pure - Tutorials"
    ]

listing model =
  Div <| Theme ArticleT |>
    ( H1 <| Theme HeaderT |>
      [ "Tutorials" ]
    : [ tutorialMeta tm
      | tm <- Shared.tutMetas (cache model)
      ]
    )

tutorialMeta Shared.TutorialMeta {..} =
  let
    ref = "/tut/" <> slug
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      ]

tutorial s model =
  Div <| Theme ArticleT |>
    [ case lookup s (Shared.tutorials (cache model)) of
        Just (Done t) -> success t
        Just Failed   -> failed
        _             -> loading
    ]

failed = 
  Div <| Theme FailedT |> 
    [ "Could not find tutorial" ]

success t =
  Div <| Theme MarkdownT |> 
    (fmap captureLocalRefs (Shared.content t))

loading =
  Div <| Theme LoadingT |>
    [ "Loading tutorial" ]
