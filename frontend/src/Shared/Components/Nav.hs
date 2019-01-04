module Shared.Components.Nav where

import Pure
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties
import Pure.Router
import Pure.Theme

import Shared.Colors

import Scope hiding (none,has)

nav :: PageScope => View
nav = withState $ \(Scope.view activePage -> active) ->
  Nav <| Theme NavT |>
    [ navLink (Just x == active) l t
    | (x,l,t) <-
      [ (BlogActive,"/blog","Blog")
      , (DocsActive,"/docs","Docs")
      , (TutsActive,"/tuts","Tutorial")
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

      atMedia "(max-width: 48em)" .> do
        width        =: per 80
        alignItems   =: start

navLink active link text =
  A <| lref link . Theme NavLinkT |>
    [ text
    , if active then Span else Null
      -- existence of span is a highlight
    ]

data NavLinkT = NavLinkT
instance Themeable NavLinkT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        display       =: flex
        flexDirection =: row
        alignItems    =: center
        alignContent  =: spaceBetween
        height         =: per 100
        paddingLeft    =: pxs 20
        paddingRight   =: pxs 20
        position       =: relative
        fontSize       =: pxs 24
        fontWeight     =: int 200
        color          =: white
        textDecoration =: none

      has "span" .> do
        position        =: absolute
        height          =: pxs 3
        left            =: zero
        right           =: zero
        bottom          =: pxs 0
        backgroundColor =: blueHighlight

      atMedia "(max-width: 48em)" .> do
        paddingLeft  =: pxs 10
        paddingRight =: pxs 10
        fontSize     =: pxs 18

