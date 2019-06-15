module Components.Header where

import Pure.Data.CSS
import Pure.Elm hiding (left,right)
import qualified Pure.Elm as Pure
import Pure.Theme

import Components.Icons
import Components.Nav
import Themes
import Types

import Control.Monad

header :: Route -> Bool -> View
header rt transparent =
  let 
    t = if transparent then Theme HeaderTransparentT else Theme Components.Header.HeaderT
    b = if transparent then barMinusLogo else bar
  in 
    Header <| t |> [ b rt ]

bar_ :: Bool -> Route -> View
bar_ b rt =
  let 
    l = if b then left else Null
    r = right rt
  in
    Div <| Theme BarT |> [ l , r ]

bar :: Route -> View
bar = bar_ True

barMinusLogo :: Route -> View
barMinusLogo = bar_ False

left :: View
left = 
  Div <| Theme LeftT |>
    [ logo False True HeaderLogoT ]

right :: Route -> View
right rt =
  Div <| Theme RightT |>
    [ nav rt
    , gitHubLink "https://github.com/grumply/pure" HeaderGitHubLinkT
    ]

data HeaderT = HeaderT
instance Themeable Components.Header.HeaderT where
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
