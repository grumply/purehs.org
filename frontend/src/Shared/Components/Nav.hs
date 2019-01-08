module Shared.Components.Nav where

import Pure
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties
import Pure.Router
import Pure.Theme

import Containers.Blog as Blog
import Containers.Docs as Docs
import Containers.Examples as Examples
import Containers.Tutorials as Tuts
import Shared.Colors

import Scope hiding (none,has)

nav :: PageScope => View
nav = withRoute $ \rt ->
  Nav <| Theme NavT |>
    [ navLink (Just x == (toPageType rt)) l t f
    | (f,x,l,t) <-
      [ (Blog.fetcher,BlogPage,"/blog","Blog")
      , (Docs.fetcher,DocsPage,"/docs","Docs")
      , (Examples.fetcher,ExamplesPage,"/examples","Examples")
      , (Tuts.fetcher,TutsPage,"/tuts","Tutorials")
      ]
    ]

data NavT = NavT
instance Themeable NavT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        display        =: flex
        flexDirection  =: row
        overflowX      =: auto
        justifyContent =: flexEnd

navLink active link text f =
  A <| lref link . Theme NavLinkT |>
    [ text
    , if active then Span else Null
      -- existence of span is a highlight
    , f
    ]

data NavLinkT = NavLinkT
instance Themeable NavLinkT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        display         =: flex
        flexDirection   =: row
        alignItems      =: center
        alignContent    =: spaceBetween
        height          =: per 100
        paddingLeft     =: pxs 10
        position        =: relative
        fontSize        =: pxs 24
        fontWeight      =: int 200
        color           =: white
        textDecoration  =: none

      has "span" .> do
        position        =: absolute
        height          =: pxs 1
        left            =: pxs 10
        right           =: zero
        bottom          =: pxs 0
        backgroundColor =: blueHighlight

      atMedia "(max-width: 48em)" .> do
        fontSize        =: pxs 18

      is ":hover" .> do
        textShadow      =: zero <<>> zero <<>> pxs 5 <<>> lightGreen

