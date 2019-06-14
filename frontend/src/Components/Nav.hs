module Components.Nav where

import Pure.Data.CSS
import Pure.Elm
import Pure.Theme

import Colors
import Types

import Control.Monad

nav :: Route -> View
nav rt = 
  Nav <| Theme NavT |>
    [ navLink (active rt) l t
    | (active,l,t) <-
      [ (isAboutRoute,"/about","About")
      , (isBlogRoute,"/blog","Blog")
      , (isDocsRoute,"/docs","Docs")
      , (isTutorialsRoute,"/tuts","Tutorials")
      ]
    ]

navLink active link text =
  A <| lref link . Theme LinkT |>
    [ text
    , if active then Span else Null
      -- existence of span is a highlight
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

data LinkT = LinkT
instance Themeable LinkT where
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

