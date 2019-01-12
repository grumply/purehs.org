{-# LANGUAGE QuasiQuotes #-}
module Pages.Blog (blogPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.Txt as Txt
import Pure.Data.Txt.Interpolate
import Pure.Router
import Pure.Theme

import Containers.Blog
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

blogPage :: PageScope => View
blogPage =
  Div <| Theme BlogPageT . Theme PageT |>
    [ header
    , Div <| Theme BlogContainerT |>
      [ H1 <| Theme BlogHeaderT |>
        [ "Posts" ]
      , container loading postMetas
      ]
    , fetcher
    ]

loading =
  Div <| Theme LoadingT

postMetas pms =
  Div <| Theme PostsT |> (fmap postMeta pms)

postMeta PostMeta {..} =
  let
    ref = [i|/blog/#{slug}|]
  in
    Div <| Theme PostT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      , Div <| Theme DateT  |> [ text year, "-", text month, "-" , text day ]
      ]

data BlogPageT = BlogPageT
instance Themeable BlogPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data BlogContainerT = BlogContainerT
instance Themeable BlogContainerT where
  theme c _ = void $
    is c .> do
      width       =: per 100
      maxWidth    =: pxs 1200
      marginLeft  =: auto
      marginRight =: auto
      padding     =: ems 1

data BlogHeaderT = BlogHeaderT
instance Themeable BlogHeaderT where
  theme c _ = void $ is c .> do
    fontSize =: ems 3
    color =: darkGray

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data PostsT = PostsT
instance Themeable PostsT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data PostT = PostT
instance Themeable PostT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data DateT = DateT
instance Themeable DateT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 1
      color =: darkGray
