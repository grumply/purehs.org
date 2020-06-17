module Components.Header where

import qualified App
import Components.Icons
import Components.Nav
import Data.Route
import Styles.Themes hiding (nav)
import Styles.Responsive
import Styles.Colors

import Pure.Data.SVG (pattern Svg)
import Pure.Elm hiding (nav,lavender)
import qualified Pure.Elm as Pure

import Prelude hiding (max)

{-# NOINLINE header #-}
header :: App.App => Route -> View
header rt =
  let 
    transp | HomeR <- rt = True
           | otherwise   = False
    t = if transp then id else Themed @SolidHeaderT
    b = if transp then barMinusLogo else bar
  in 
    Header <| Themed @HeaderBaseT . t |> 
      [ b rt ]

bar_ :: App.App => Bool -> Route -> View
bar_ b rt =
  Div <| Themed @BarT |> 
    [ if b then left else Null 
    , right 
    ]
  where
    left = 
      Section <| Themed @LeftT |>
        [ logo @HeaderLogoT False True ]

    right =
      Section <| Themed @RightT |>
        [ nav rt
        , gitHubLink @HeaderGitHubLinkT "https://github.com/grumply/pure-platform" 
        ]

bar :: App.App => Route -> View
bar = bar_ True

barMinusLogo :: App.App => Route -> View
barMinusLogo = bar_ False

data SolidHeaderT
instance Theme SolidHeaderT where
  theme c = void $ is c .> do
    background-color =: toTxt lavender 

data HeaderBaseT
instance Theme HeaderBaseT where
  theme c = void $ is c .> do
    position  =: absolute
    top       =: 0
    left      =: 0
    z-index   =: 100
    width     =: (100%)

data BarT = BarT
instance Theme BarT where
  theme c = void $ is c $ do
    apply $ do
      padding-left    =: 15px
      padding-right   =: 15px
      margin-left     =: auto
      margin-right    =: auto
      height          =: 40px
      display         =: flex
      align-items     =: center
      justify-content =: space-between
      flex-direction  =: row

    mediumScreens <%> do
      padding-left  =: 20px
      padding-right =: 20px
      height        =: 60px

    largeScreens <%> do
      width =: (90%) 

    hugeScreens <%> do
      max-width =: 1260px

data LeftT = LeftT
instance Theme LeftT where
  theme c = void $ is c .> do
    display         =: flex
    justify-content =: flex-start

data HeaderLogoT = HeaderLogoT
instance Theme HeaderLogoT where
  theme c = void $ is c $ do
    apply $ 
      width =: 85px

    mediumScreens <%> do
      width =: 100px

data RightT
instance Theme RightT where
  theme c = void $
    is c .> do
      display         =: flex
      width           =: (100%)
      justify-content =: flex-end

data HeaderGitHubLinkT
instance Theme HeaderGitHubLinkT where
  theme c = void $
    is c $ do
      apply $ do
        display     =: none
        margin-left =: 20px
        fill        =: white

      mediumScreens <%> do
        display =: initial

      has (tag Svg) .> do
        margin-left =: auto
        width       =: 30px
