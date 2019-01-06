module Pages.Home (homePage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties
import Pure.Router
import Pure.Theme

import Shared.Colors
import Shared.Components.Header
import Shared.Components.Logo
import Shared.Styles

import Scope hiding (has,none,transform)

homePage :: PageScope => View
homePage =
  Div <| Theme HomeT . Theme PageT |>
    [ header
    , Div <| Theme IntroT |>
      [ Div <| Theme HeroT |>
        [ logo False False HeroLogoT
        , H1 <| Theme SloganT |>
          [ Span <||> [ "The web from a " ]
          , Span <||> [ I <||> [ "different angle." ] ]
          ]
        , P <| Theme DescriptionT |>
          [ "Pure is a Haskell-based web stack that focuses on"
          , Br
          , "performance, expressiveness, and asynchrony."
          ]
        , Div <| Theme CallToActionT |>
          [ A <| lref "/docs" . Theme GetPureT |>
            [ "Get Pure" ]
          , A <| lref "/tuts" . Theme StartTutorialT |>
            [ "Start Tutorial" ]
          ]
        ]
      ]
    ]

data HomeT = HomeT
instance Themeable HomeT where
  theme c _ = void $ do
    is c .> do
      let gradient = deg 150                   <&>>
                     darkLavender  <<>> per 15 <&>>
                     blueHighlight <<>> per 70 <&>>
                     lightGreen    <<>> per 95

      minHeight       =: per 100
      display         =: flex
      flexDirection   =: column
      background      =: linearGradient(gradient)

    atMedia "(max-height: 500px)" $
      is c .> do
        paddingTop    =: ems 3
        paddingBottom =: ems 3

data IntroT = IntroT
instance Themeable IntroT where
  theme c _ = void $ do
    is c .> do
      display    =: flex
      flex       =: one
      color      =: baseWhite
      alignItems =: center

    is c $
      headerOffset

data HeroT = HeroT
instance Themeable HeroT where
  theme c _ = void $ do
    is c .> do
      display       =: flex
      flex          =: one
      flexDirection =: column
      textAlign     =: center
      fontWeight    =: int 200

data HeroLogoT = HeroLogoT
instance Themeable HeroLogoT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        margin =: auto
        width  =: per 60

      atMedia "(max-width: 48em)" .> do
        width  =: per 80

data SloganT = SloganT
instance Themeable SloganT where
  theme c _ = void $ do
    is c .> do
      marginTop      =: ems 0.5
      marginBottom   =: ems 0.25
      display        =: flex
      flex           =: one
      flexWrap       =: wrap
      justifyContent =: center
      flexDirection  =: row
      fontWeight     =: int 200
      fontSize       =: pxs 40
      textShadow     =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

    is c . has "span" .> do
      display        =: flex
      marginLeft     =: ems 0.25
      justifyContent =: center

data DescriptionT = DescriptionT
instance Themeable DescriptionT where
  theme c _ = void $ do
    is c .> do
      display        =: "inline-flex"
      flex           =: one
      justifyContent =: center
      fontSize       =: pxs 16
      textShadow     =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

buttonBoxShadow opacity vOff blur vOff' blur' =
       zero <<>> pxs vOff  <<>> pxs blur  <<>> darkLavender
  <&>> zero <<>> pxs vOff' <<>> pxs blur' <<>> rgba(0,0,0,0.1)

callToActionButtonStyles background text = do
  apply $ do
    transition      =: "all" <<>> sec 0.1 <<>> easeInOut
    backgroundColor =: background
    color           =: text

  is hovered .> do
    transform       =: scale(dec 1.1) <<>> translateY(pxs (-3))
    boxShadow       =: buttonBoxShadow 0.11 10 14 3 6

  is active .> do
    transform       =: scale(dec 0.9) <<>> translateY(pxs 5)
    boxShadow       =: buttonBoxShadow 0.13 4 6 1 3

data CallToActionT = CallToActionT
instance Themeable CallToActionT where
  theme c _ = void $ do
    is c .> do
      height           =: pxs 50
      display          =: flex
      flexDirection    =: row
      justifyContent   =: center

    is c . has "a" .> do
      display          =: inlineBlock
      marginLeft       =: pxs 16
      marginRight      =: pxs 16
      height           =: pxs 40
      lineHeight       =: pxs 40
      padding          =: zero <<>> pxs 14
      boxShadow        =: buttonBoxShadow 0.13 4 6 1 3
      borderRadius     =: pxs 5
      fontWeight       =: int 600
      textTransform    =: uppercase
      "letter-spacing" =: ems 0.025
      textDecoration   =: none

data GetPureT = GetPureT
instance Themeable GetPureT where
  theme c _ = void $ is c $ callToActionButtonStyles baseGreen baseWhite

data StartTutorialT = StartTutorialT
instance Themeable StartTutorialT where
  theme c _ = void $ is c $ callToActionButtonStyles baseWhite darkLavender
