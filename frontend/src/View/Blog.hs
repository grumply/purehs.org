module View.Blog where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Router (lref)
import Pure.Theme

import Shared.Cache as Cache (Cache(posts,postMetas))
import Shared.Post as Post (Post(content),Meta(..))
import Themes
import Types
import Utils

import Components.Header (header)
import Components.Icons  (logo)
import Components.Titler (titler)
import Components.With

import Control.Monad

blog :: Model -> View
blog model =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
        [ case route model of
            BlogR   -> listing model
            PostR s -> post s model
        ]
    , titler $
        case route model of
          PostR s -> "Pure - " <> s
          _       -> "Pure - Blog"
    ]

listing model =
  Div <| Theme ArticleT |>
    ( H1 <| Theme HeaderT |>
      [ "Blog" ]
    : [ postMeta pm
      | pm <- Cache.postMetas (cache model)
      ]
    )

postMeta Post.Meta {..} =
  let
    ref = "/blog/" <> slug
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      , Div <| Theme DateT  |> [ text year, "-", text month, "-" , text day ]
      ]

post s model =
  Div <| Theme ArticleT |>
    [ case lookup s (Cache.posts (cache model)) of
        Just (Done p) -> success p
        Just Failed   -> failed
        Just Trying   -> loading
        Nothing       -> with (publish (LoadPost s)) loading
    ]

failed =
  Div <| Theme FailedT |>
    [ "Could not find post" ]

success p =
  Div <| Theme MarkdownT |>
    (fmap captureLocalRefs (Post.content p))

loading =
  Div <| Theme LoadingT |>
    [ "Loading post" ]

data DateT = DateT
instance Themeable DateT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 1
      color =: darkGray
