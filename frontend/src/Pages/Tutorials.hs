module Pages.Tutorials where

import qualified App 
import Components.Header (header)
import Data.Route as Route
import Styles.Themes

import Shared.Cache as Cache (Cache(tutMetas))
import Shared.Tutorial as Tutorial (Meta(..))

import Pure.Elm.Application as Elm

tutorials :: App.App => View
tutorials =
  Div <| Theme PageT |>
    [ header TutorialsR False
    , listing session
    ]

listing model =
  Div <| Theme ContentT |>
    [ tutorialMeta tm
    | tm <- Cache.tutMetas (App.cache model)
    ]

tutorialMeta Tutorial.Meta {..} =
  Div <| Theme MetaT . link (TutorialR slug) |>
    [ Div <| Theme TitleT |> [ text title ]
    ]

data MetaT = MetaT
instance Themeable MetaT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ is c .> do
    fontSize =: ems 2

