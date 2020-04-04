module Pages.Tutorials.Tutorial where

import qualified App 
import Components.Header (header)
import Control.With
import Data.Route as Route
import Styles.Themes

import Shared.Cache as Cache (Cache(tutorials))
import Shared.Tutorial as Tutorial (Tutorial(content))

import Pure.Data.Try
import Pure.Elm.Application as Elm

tutorial :: App.App => Txt -> View
tutorial s =
  Div <| Theme PageT |>
    [ header (TutorialR s) False
    , Div <| Theme ContentT |> 
      [ case lookup s (Cache.tutorials (App.cache session)) of
          Just (Done t) -> success t
          Just Failed   -> failed
          Just Trying   -> loading
          Nothing       -> with s (App.loadTutorial s) loading
      ]
    ]

failed =
  Div <| Theme FailedT |>
    [ "Could not find tutorial" ]

success t =
  Div <| Theme MarkdownT |>
    fmap processLinks (Tutorial.content t)

loading =
  Div <| Theme LoadingT |>
    [ "Loading tutorial" ]

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()

