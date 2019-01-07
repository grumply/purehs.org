module Shared.Components.Header where

import Pure hiding (left,right)
import qualified Pure
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties
import Pure.Router
import Pure.Theme

import Shared.Colors
import Shared.Components.GitHubLogo
import Shared.Components.Logo
import Shared.Components.Nav

import Scope hiding (has,none)

header :: PageScope => View
header = Header <| Theme HeaderT |> [ bar ]

headerTransparent :: PageScope => View
headerTransparent = Header <| Theme HeaderTransparentT |> [ barMinusLogo ]

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ do
    is c .> do
      position        =: absolute
      top             =: zero
      Pure.left       =: zero
      zIndex          =: int 100
      width           =: per 100
      backgroundColor =: hsla(250,49,49,0.9)

data HeaderTransparentT = HeaderTransparentT
instance Themeable HeaderTransparentT where
  theme c _ = void $ do
    is c .> do
      position        =: absolute
      top             =: zero
      Pure.left       =: zero
      zIndex          =: int 100
      width           =: per 100

bar :: PageScope => View
bar =
  Div <| Theme BarT |>
    [ left
    , right
    ]

barMinusLogo :: PageScope => View
barMinusLogo =
  Div <| Theme BarT |>
    [ right ]

data BarT = BarT
instance Themeable BarT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        paddingLeft   =: pxs 20
        paddingRight  =: pxs 20
        marginLeft    =: auto
        marginRight   =: auto
        height        =: pxs 60
        display       =: flex
        alignItems    =: center
        justifyContent =: spaceBetween
        flexDirection =: row

      atMedia "(max-width: 48em)" .> do
        paddingLeft =: pxs 15
        paddingRight =: pxs 15
        height =: pxs 40

      atMedia "(min-width: 780px)" .> do
        width =: per 90

      atMedia "(min-width: 1340px)" .> do
        maxWidth =: pxs 1260

headerOffset = do
    apply $ do
      marginTop =: pxs 75

    atMedia "(max-width: 48em)" .> do
      marginTop =: pxs 50

left =
  Div <| Theme LeftT |>
    [ logo False True HeaderLogoT ]

data LeftT = LeftT
instance Themeable LeftT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      justifyContent =: flexStart

data HeaderLogoT = HeaderLogoT
instance Themeable HeaderLogoT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        width  =: pxs 100

      atMedia "(max-width: 48em)" .> do
        width  =: pxs 85

right :: PageScope => View
right =
  Div <| Theme RightT |>
    [ nav
    , gitHubLink "https://github.com/grumply/pure" HeaderGitHubLinkT
    ]

data RightT = RightT
instance Themeable RightT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      width =: per 100
      justifyContent =: flexEnd

data HeaderGitHubLinkT = HeaderGitHubLinkT
instance Themeable HeaderGitHubLinkT where
  theme c _ = void $
    is c $ do
      apply $ do
        marginLeft =: pxs 20
        fill =: white

      atMedia "(max-width: 500px)" .> do
        display =: none

      has "svg" .> do
        marginLeft =: auto
        width  =: pxs 30
