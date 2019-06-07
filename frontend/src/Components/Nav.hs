module Components.Nav where

import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties

import Colors
import Context
import Imports

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env

data State = State

newtype NavM a = NavM { runNavM :: Aspect (Ctx NavM) Env State a }
mkAspect ''NavM

viewNav :: Ctx NavM -> View
viewNav c = viewNavM nav c Env State

nav :: NavM View
nav = do
  rt <- getRoute
  pure $
    Nav <| Theme NavT |>
      [ navLink (active rt) l t
      | (active,l,t) <-
        [ (isBlogRoute,"/blog","Blog")
        , (isDocsRoute,"/docs","Docs")
        , (isExamplesRoute,"/examples","Examples")
        , (isTutorialsRoute,"/tuts","Tutorials")
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

navLink active link text =
  A <| lref link . Theme LinkT |>
    [ text
    , if active then Span else Null
      -- existence of span is a highlight
    ]

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

