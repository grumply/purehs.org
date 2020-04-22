module Pages.Blog where

import qualified App
import Components.Header (header)
import Data.Route as Route
import Shared.Cache as Cache (Cache(postMetas))
import Shared.Post as Post (Meta(..))
import Styles.Colors
import Styles.Themes

import Pure.Elm.Application hiding (page,meta,Meta,Frame)

page :: App.App => View
page =
  Page $
    WithHeader (header BlogR False) $
      WithoutSidebar $
        Div <||> 
          [ meta pm | pm <- Cache.postMetas (App.cache session) ]

meta :: Meta -> View
meta Meta {..} =
  Div <| Theme MetaT . link (PostR slug) |>
    [ Div <| Theme TitleT |> [ text title ]
    , Div <| Theme DateT  |> [ text year, "-", text month, "-" , text day ]
    ]

data MetaT = MetaT
instance Themeable MetaT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ is c .> do
    fontSize =: ems 2

data DateT = DateT
instance Themeable DateT where
  theme c _ = void $ is c .> do
    fontSize =: ems 1
    color =: darkGray

