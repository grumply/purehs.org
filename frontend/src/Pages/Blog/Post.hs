module Pages.Blog.Post where

import qualified App 
import Components.Header (header)
import Control.With
import Data.Route
import Shared.Cache as Cache (Cache(posts))
import Shared.Post as Post (Post(content))
import Styles.Themes

import Pure.Data.Try
import Pure.Elm.Application as Elm

post :: App.App => Txt -> View
post s =
  Div <| Theme PageT |>
    [ header BlogR False
    , posting s
    ]

posting :: App.App => Txt -> View
posting s =
  case lookup s (Cache.posts (App.cache session)) of
    Just (Done p) -> success p
    Just Failed   -> failed
    Just Trying   -> loading
    Nothing       -> with s (App.loadPost s) loading

failed :: View
failed =
  Div <| Theme FailedT |>
    [ "Could not find post" ]

success :: App.App => Post -> View
success p =
  Div <| Theme MarkdownT |>
    (fmap processLinks (Post.content p))

loading :: View
loading =
  Div <| Theme LoadingT |>
    [ "Loading post" ]

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()
