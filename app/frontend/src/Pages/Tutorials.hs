module Pages.Tutorials (page) where

import qualified App 
import Components.Header (header)
import Data.Route as Route
import Styles.Themes

import Shared.Cache as Cache (Cache(tutMetas))
import Shared.Tutorial as Tutorial (Meta(..))

import Pure.Elm.Application as Elm hiding (page,meta,Meta,Frame)

page :: App.App => View
page =
  Page $
    WithHeader (header TutorialsR False) $
      WithoutSidebar $
        Div <||>
          [ meta tm
          | tm <- Cache.tutMetas (App.cache session)
          ]

meta :: App.App => Tutorial.Meta -> View
meta Meta {..} =
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

