module Pages.Blog where

import qualified App
import Components.Header (header)
import Data.Route as Route
import Shared.Cache as Cache (Cache(postMetas))
import Shared.Post as Post (Meta(..))
import Styles.Colors
import Styles.Themes

import Pure.Elm.Application

blog :: App.App => View
blog =
  Div <| Theme PageT |>
    [ header BlogR False
    , listing 
    ]

listing :: App.App => View
listing =
  Div <| Theme ContentT |>
    [ postMeta pm
    | pm <- Cache.postMetas (App.cache session)
    ]

postMeta :: Meta -> View
postMeta Post.Meta {..} =
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

