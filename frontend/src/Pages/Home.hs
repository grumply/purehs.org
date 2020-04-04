module Pages.Home where

import qualified App
import Components.Header (header)
import Components.Icons  (logo)
import Data.Route
import Styles.Colors
import Styles.Themes hiding (ContentT)

import Pure.Elm.Application as Elm

home :: App.App => View
home =
  Div <| Theme HomeT |>
    [ header HomeR True
    , Pages.Home.content 
    ]

content :: App.App => View
content = 
  Div <| Theme ContentT |>
    [ Div <| Theme IntroT |>
      [ Div <| Theme HeroT |>
        [ logo False False HeroLogoT
        , H1 <| Theme SloganT |>
          [ "Dynamic Hierarchical Contexts"
          ]
        , P <| Theme DescriptionT |>
          [ "Performance + Expressiveness + Asynchrony"
          ]
        , Div <| Theme CallToActionT |>
          [ A <| link (TutorialR "install") . Theme GetPureT |>
            [ "Get Pure" ]
          , A <| link (TutorialR "basics") . Theme StartTutorialT |>
            [ "Start Tutorial" ]
          ]
        ]
      ]
    , Div <| Theme GradientT
    ]

data HomeT = HomeT
instance Themeable HomeT where
  theme c _ = void $ do
    is c .> do
      height          =: per 100

    atMedia "(max-height: 500px)" $
      is c .> do
        paddingTop    =: pxs 48
        paddingBottom =: pxs 48

data GradientT = GradientT
instance Themeable GradientT where
  theme c _ = void $ do

    atKeyframes "shimmer" $ do
      is (per   0) .> opacity =: one
      is (per 100) .> opacity =: zero


    is c .> do
      let gradient = deg 150                   <&>>
                     darkLavender  <<>> per 15 <&>>
                     blueHighlight <<>> per 70 <&>>
                     lightGreen    <<>> per 95

      position        =: absolute
      height          =: per 100
      width           =: per 100
      top             =: zero
      left            =: zero
      zIndex          =: neg (int 100)
      opacity         =: one
      background      =: linearGradient(gradient)

    is c . is ":before" .> do
      let gradient = deg 210                   <&>>
                     darkLavender  <<>> per 15 <&>>
                     blueHighlight <<>> per 70 <&>>
                     lightGreen    <<>> per 95

      Elm.content     =: "\"\""
      position        =: absolute
      display         =: block
      height          =: per 100
      width           =: per 100
      top             =: zero
      left            =: zero
      zIndex          =: neg (int 99)
      opacity         =: zero
      background      =: linearGradient(gradient)
      animation       =: "shimmer" <<>> sec 3 <<>> easeInOut <<>> normal

    atMedia "(max-height: 500px)" $
      is c .> do
        paddingTop    =: pxs 48
        paddingBottom =: pxs 48

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $ do

    is c .> do
      display         =: flex
      flexDirection   =: column
      justifyContent  =: center
      height          =: per 100

data IntroT = IntroT
instance Themeable IntroT where
  theme c _ = void $ do
    is c .> do
      display    =: flex
      flex       =: one
      color      =: baseWhite
      alignItems =: center

    is c $ do
      apply $ marginTop =: pxs 75

      atMedia "(max-width: 48em)" .> do
        marginTop =: pxs 50


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
        width  =: "90vmin"

data SloganT = SloganT
instance Themeable SloganT where
  theme c _ = void $ do
    is c .> do
      marginTop      =: pxs 8
      marginBottom   =: pxs 4
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
      marginLeft     =: pxs 4
      justifyContent =: center

    atMedia "(max-width: 48em)" $
      is c .> do
        fontSize =: pxs 30

data DescriptionT = DescriptionT
instance Themeable DescriptionT where
  theme c _ = void $ do
    is c .> do
      display        =: "inline-flex"
      flex           =: one
      justifyContent =: center
      fontSize       =: pxs 24
      textShadow     =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

    atMedia "(max-width: 48em)" $
      is c .> do
        fontSize =: pxs 22

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
      marginTop        =: pxs 8
      marginBottom     =: pxs 8

    is c . has "a" .> do
      display          =: inlineBlock
      marginLeft       =: pxs 16
      marginRight      =: pxs 16
      height           =: pxs 44
      lineHeight       =: pxs 44
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
