module Pages.Home (page,GradientT(..)) where

import qualified App
import Components.Header (header)
import Components.Icons  (logo,animatedLogo)
import Components.Preload (prelink)
import Data.Route
import Styles.Colors
import Styles.Themes hiding (ContentT,page)

import Pure.Elm.Application as Elm hiding (home,page,green,lavender,blue,brightness,teal)

import Prelude hiding (all,min,max)

page :: App.App => View
page =
  Div <| Themed @HomeT |>
    [ header HomeR
    , home
    ]

home :: App.App => View
home = 
  Div <| Themed @ContentT |>
    [ Div <| Themed @IntroT |>
      [ Div <| Themed @HeroT |>
        [ logo @HeroLogoT False False 
        , H1 <| Themed @SloganT |>
          [ "Dynamic Hierarchical Contexts" ]
        , P <| Themed @DescriptionT |>
          [ "Performance + Expressiveness + Asynchrony" ]
        , Div <| Themed @CallToActionT |>
          [ A <| prelink (PageR "about") . Themed @ButtonT . Themed @AboutPureT |>
            [ "About Pure.hs" ]
          , A <| prelink (TutorialR "install") . Themed @ButtonT . Themed @GetPureT |>
            [ "Get Pure.hs" ]
          ]
        ]
      ]
    , Div <| Themed @GradientT
    ]

data HomeT
instance Theme HomeT where
  theme c = void $ do
    is c .> do
      height =: (100%)

    atMedia "(max-height: 500px)" $
      is c .> do
        padding-top    =: 24px
        padding-bottom =: 48px

data GradientT
instance Theme GradientT where
  theme c = do
    is c do
      position   =: absolute
      min-height =: (100%)
      width      =: 100vw
      top        =: 0
      left       =: 0
      z-index    =: (-100)
      opacity    =: 1
      background =: linearGradient
        [150deg
        ,toTxt blue     <<>> (10%)
        ,toTxt lavender <<>> (65%)
        ,toTxt green    <<>> (95%)
        ]

      before do
        content    =: emptyQuotes
        position   =: absolute
        display    =: block
        min-height =: (100%)
        width      =: 100vw
        top        =: 0
        left       =: 0
        z-index    =: (-99)
        opacity    =: 0
        background =: linearGradient
          [210deg
          ,toTxt blue     <<>> (10%)
          ,toTxt lavender <<>> (65%)
          ,toTxt teal     <<>> (95%)
          ]
        animation  =: "shimmer" <<>> 3s <<>> easeinout <<>> normal

      atMedia "(max-height: 500px)" do
        padding-top    =: 24px
        padding-bottom =: 48px

    atKeyframes "shimmer" do
      is (0%) do
        opacity =: 1

      is (100%) do 
        opacity =: 0


data ContentT
instance Theme ContentT where
  theme c = 
    is c do
      display         =: flex
      flex-direction  =: column
      justify-content =: center
      height          =: (100%)

data IntroT
instance Theme IntroT where
  theme c = 
    is c do
      display     =: flex
      flex        =: 1
      color       =: toTxt base
      align-items =: center
      margin-top =: 75px

      atMedia "(max-width: 48em)" do
        margin-top =: 50px


data HeroT
instance Theme HeroT where
  theme c = 
    is c do
      display        =: flex
      flex           =: 1
      flex-direction =: column
      text-align     =: center
      font-weight    =: 200
      max-width      =: (100%)

data HeroLogoT
instance Theme HeroLogoT where
  theme c =
    is c do
      margin =: auto
      width  =: 90vmin

data SloganT
instance Theme SloganT where
  theme c =
    is c do
      margin-top      =: 8px
      margin-bottom   =: 4px
      display         =: flex
      flex            =: 1
      flex-wrap       =: wrap
      justify-content =: center
      flex-direction  =: row
      font-weight     =: 200
      font-size       =: 40px
      text-shadow     =* [1px,1px,hsla(215,(35.14%),(40%),0.5)]

      has "span" do
        display         =: flex
        margin-left     =: 4px
        justify-content =: center

      atMedia "(max-width: 48em)" do
        font-size =: 30px

data DescriptionT
instance Theme DescriptionT where
  theme c =
    is c do
      display         =: inline-flex
      flex            =: 1
      justify-content =: center
      font-size       =: 24px
      text-shadow     =* [1px,1px,hsla(215,(35.14%),(40%),0.5)]

      atMedia "(max-width: 48em)" do
        font-size =: 22px

data CallToActionT
instance Theme CallToActionT where
  theme c =
    is c do
      height          =: 50px
      display         =: flex
      flex-direction  =: row
      justify-content =: center
      margin-top      =: 8px
      margin-bottom   =: 8px

data AboutPureT
instance Theme AboutPureT where
  theme c = 
    is c do
      callToActionButtonStyles (toTxt green { brightness = 80 }) (toTxt base)

data GetPureT
instance Theme GetPureT where
  theme c = 
    is c do
      callToActionButtonStyles (toTxt base) (toTxt lavender)

callToActionButtonStyles bkg text = do
  transition       =* [all,0.1s,easeinout]
  background-color =: bkg
  color            =: text
  box-shadow       =: buttonBoxShadow 3 4 (toTxt lavender { brightness = 80 }) 1 3 (rgba(0,0,0,0.11))

  hover do
    color      =: text
    transform  =* [scale(1.1),translateY((-3)px)]
    box-shadow =: buttonBoxShadow 10 14 (toTxt lavender { brightness = 40 }) 3 6 (rgba(0,0,0,0.11))

  active do
    color      =: text
    transform  =* [scale(0.9),translateY(5px)]
    box-shadow =: buttonBoxShadow 4 6 (toTxt lavender { brightness = 40 }) 1 3 (rgba(0,0,0,0.13))

