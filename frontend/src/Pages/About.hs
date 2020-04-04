module Pages.About where

import qualified App
import Components.Header
import Control.With
import Data.Route as Route
import Shared.Cache as Cache (Cache(pages))
import Shared.Page as Page (Page(content))
import Styles.Themes

import Pure.Data.Try
import Pure.Elm.Application hiding (content,page)

about :: App.App => View
about =
  Div <| Theme PageT |>
    [ header AboutR False
    , page
    ]

page :: App.App => View
page =
  Div <| Theme ContentT |> 
    [ case lookup "about" (Cache.pages (App.cache session)) of
        Just (Done pg) -> success pg
        Just Trying    -> loading
        Just Failed    -> notFound
        Nothing        -> with AboutR (App.loadPage "about") loading
    ]

success :: App.App => Page.Page -> View
success pg =
  Div <| Theme MarkdownT |>
    fmap processLinks (Page.content pg)

loading :: View
loading =
  Div <| Theme LoadingT |>
    [ "Loading Page" ]

notFound :: View
notFound =
  Div <| Theme FailedT |>
    [ "Page not found." ] 

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()
