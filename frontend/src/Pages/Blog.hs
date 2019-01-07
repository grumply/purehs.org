{-# LANGUAGE QuasiQuotes #-}
module Pages.Blog (blogPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
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
      [ container loading postMetas
      ]
    ]

loading =
  Div <| Theme LoadingT

postMetas pms =
  Div <| Theme PostsT |> (fmap postMeta pms)

postMeta pm =
  let
    (y,m,d,s) = pmPath pm
    ref = [i|/blog/#{y}/#{m}/#{d}/#{s}|]
  in
    Div <| Theme PostT . lref ref |>
      [ Div <| Theme TitleT  |> [ text (pmTitle pm) ]
      , Div <| Theme AuthorT |> [ text (pmAuthor pm) ]
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
  theme c _ = void $ do
    is c $ do
      headerOffset

      apply $ do
        maxWidth   =: pxs 1200
        margin     =: auto
        padding    =: ems 1

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data PostsT = PostsT
instance Themeable PostsT where
  theme c _ = void $ is c $ return ()

data PostT = PostT
instance Themeable PostT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ is c $ return ()

data AuthorT = AuthorT
instance Themeable AuthorT where
  theme c _ = void $ is c $ return ()
