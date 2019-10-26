module View.Tutorials where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Router (lref)
import Pure.Theme

import Shared.Cache as Cache (Cache(tutMetas,tutorials))
import Shared.Tutorial as Tutorial (Tutorial(content),Meta(..))
import Themes
import Types
import Utils

import Components.Header (header)
import Components.Icons  (logo)
import Components.Titler (titler)
import Components.With

import Control.Monad

tutorials :: Model -> View
tutorials model =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
      [ case route model of
          TutorialsR  -> listing model
          TutorialR t -> tutorial t model
      ]
    , titler $
        case route model of
          TutorialR t -> "Pure - " <> t -- ! FIXME: this is a slug, not a title
          _           -> "Pure - Tutorials"
    ]

listing model =
  Div <| Theme ArticleT |>
    ( H1 <| Theme HeaderT |>
      [ "Tutorials" ]
    : [ tutorialMeta tm
      | tm <- Cache.tutMetas (cache model)
      ]
    )

tutorialMeta Tutorial.Meta {..} =
  let
    ref = "/tut/" <> slug
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      ]

tutorial s model =
  Div <| Theme ArticleT |>
    [ case lookup s (Cache.tutorials (cache model)) of
        Just (Done t) -> success t
        Just Failed   -> failed
        Just Trying   -> loading
        Nothing       -> with (publish (LoadTutorial s)) loading
    ]

failed =
  Div <| Theme FailedT |>
    [ "Could not find tutorial" ]

success t =
  Div <| Theme MarkdownT |>
    (fmap captureLocalRefs (Tutorial.content t))

loading =
  Div <| Theme LoadingT |>
    [ "Loading tutorial" ]
