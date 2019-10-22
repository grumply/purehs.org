module View.Docs where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Router (lref)
import Pure.Theme

import qualified Shared (Cache(..),Doc(..),DocMeta(..))
import Themes
import Types
import Utils

import Components.Header (header)
import Components.Icons  (logo)
import Components.Titler (titler)
import Components.With

import Control.Monad

docs :: Model -> View
docs model =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
        [ case route model of
            DocsR Nothing      -> listing model
            DocsR (Just (p,v)) -> doc p v model
        ]
    , titler $
        case route model of
          DocsR (Just (p,v)) -> "Pure - " <> p <> "-" <> v
          _ -> "Pure - Docs"
    ]

listing model = 
  Div <| Theme ArticleT |>
    ( H1 <| Theme HeaderT |>
      [ "Documentation" ]
    : [ docMeta dm
      | dm <- Shared.docMetas (cache model)
      ]
    )

docMeta Shared.DocMeta {..} =
  let
    ref = "/doc/" <> package <> "/" <> version
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme PackageT |> [ text package ]
      , Div <| Theme VersionT |> [ text version ]
      ]

doc p v model =
  Div <| Theme ArticleT |>
    [ case lookup (p,v) (Shared.docs (cache model)) of
        Just (Done d) -> success d
        Just Trying   -> loading
        Just Failed   -> failed
        Nothing       -> with (publish (LoadDoc p v)) loading
    ]

failed =
  Div <| Theme FailedT |>
    [ "Could not find doc" ]

success d =
  Div <| Theme MarkdownT |>
    (fmap captureLocalRefs (Shared.content d))

loading =
  Div <| Theme LoadingT |>
    [ "Loading doc" ]

data PackageT = PackageT
instance Themeable PackageT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data VersionT = VersionT
instance Themeable VersionT where
  theme c _ = void $ is c $ return ()

