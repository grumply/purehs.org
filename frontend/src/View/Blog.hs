module View.Blog where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Theme

import qualified Shared (Cache(..),Post(..),PostMeta(..))
import Themes
import Types
import Utils

import Components.Header (header)
import Components.Icons  (logo)
import Components.Titler (titler)

import Control.Monad

blog :: Model -> View
blog model =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False 
    , Div <| Theme ContentT |>
        [ case route model of
            BlogR Nothing  -> listing model
            BlogR (Just s) -> post s model
        ]
    , titler $
        case route model of
          BlogR (Just s) -> "Pure - " <> s
          _              -> "Pure - Blog"
    ]
          
listing model = 
  Div <| Theme ArticleT |>
    ( H1 <| Theme HeaderT |>
      [ "Blog" ]
    : [ postMeta pm 
      | pm <- Shared.postMetas (cache model) 
      ]
    )

postMeta Shared.PostMeta {..} =
  let
    ref = "/blog/" <> slug
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      , Div <| Theme DateT  |> [ text year, "-", text month, "-" , text day ]
      ]

post s model =
  Div <| Theme ArticleT |>
    [ case lookup s (Shared.posts (cache model)) of
        Just (Done p) -> success p
        Just Failed   -> failed
        _             -> loading
    ]

failed = 
  Div <| Theme FailedT |> 
    [ "Could not find post" ]

success p =
  Div <| Theme MarkdownT |> 
    (fmap captureLocalRefs (Shared.content p))

loading =
  Div <| Theme LoadingT |>
    [ "Loading post" ]

data DateT = DateT
instance Themeable DateT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 1
      color =: darkGray
