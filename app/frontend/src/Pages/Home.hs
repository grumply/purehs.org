module Pages.Home (page,Gradient) where

import qualified App
import qualified Styles.Themes as Themes
import Data.Resource
import Data.Placeholders
import Shared.Page
import qualified Components.Markdown as Markdown
import Components.Header (header)
import Components.Icons  (logo)
import Components.Preload (prelink)
import Data.Route ( Route(TutorialR, PageR, HomeR) )
import Styles.Colors ( base, blue, green, lavender, teal, Color(brightness) )
import Styles.Themes ( Button, buttonBoxShadow )

import Control.Concurrent.Async

import Pure.Elm.Application as Elm hiding (home,page,green,lavender,blue,brightness,teal,wait)
import Pure.Maybe

import Prelude hiding (all,min,max)
import Control.Monad
import Data.Maybe

page :: App.App => View
page =
  Div <| Themed @Home |>
    [ header HomeR
    , home
    , exposition
    ]

home :: App.App => View
home = 
  Div <| Themed @Content |>
    [ Div <| Themed @Intro |>
      [ Div <| Themed @Hero |>
        [ logo @HeroLogo False False 
        , H2 <| Themed @Slogan |>
          [ "Dynamic Hierarchical Contexts" ]
        , P <| Themed @Description |>
          [ "Performance + Expressiveness + Asynchrony" ]
        , Div <| Themed @CallToAction |>
          [ A <| prelink (TutorialR "5-minute") . Themed @Button . Themed @LearnPure |>
            [ "Learn Pure.hs" ]
          , A <| prelink (TutorialR "install") . Themed @Button . Themed @GetPure |>
            [ "Get Pure.hs" ]
          ]
        ]
      ]
    , Div <| Themed @Gradient
    ]

exposition :: App.App => View
exposition = producing producer (consuming consumer)
  where     
    producer = do
      PageResource ap apc <- load (PageR (fromTxt "about"))
      mp  <- wait ap
      mpc <- wait apc
      when (isNothing mp) do
        retitle "Not Found"
      pure mpc

    consumer (Just (PageContent md)) = Markdown.markdown md <| Themed @Exposition
    consumer _ = Null

data Exposition
instance Theme Exposition where
  theme c =
    is c do
      important do
        padding-top =: 2em
        padding-bottom =: 4em

data Home
instance Theme Home where
  theme c = void $ do
    is c .> do
      height =: (100%)
      box-shadow =* [0,0,15px,5px,toTxt gray]

    atMedia "(max-height: 500px)" $
      is c .> do
        padding-top    =: 24px
        padding-bottom =: 48px

data Gradient
instance Theme Gradient where
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


data Content
instance Theme Content where
  theme c = 
    is c do
      display         =: flex
      flex-direction  =: column
      justify-content =: center
      height          =: (100%)

data Intro
instance Theme Intro where
  theme c = 
    is c do
      display     =: flex
      flex        =: 1
      color       =: toTxt base
      align-items =: center
      margin-top =: 75px

      atMedia "(max-width: 48em)" do
        margin-top =: 50px


data Hero
instance Theme Hero where
  theme c = 
    is c do
      display        =: flex
      flex           =: 1
      flex-direction =: column
      text-align     =: center
      font-weight    =: 200
      max-width      =: (100%)

data HeroLogo
instance Theme HeroLogo where
  theme c =
    is c do
      margin =: auto
      width  =: 90vmin

data Slogan
instance Theme Slogan where
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

data Description
instance Theme Description where
  theme c =
    is c do
      display         =: inline-flex
      flex            =: 1
      justify-content =: center
      font-size       =: 24px
      text-shadow     =* [1px,1px,hsla(215,(35.14%),(40%),0.5)]

      atMedia "(max-width: 48em)" do
        font-size =: 22px

data CallToAction
instance Theme CallToAction where
  theme c =
    is c do
      height          =: 50px
      display         =: flex
      flex-direction  =: row
      justify-content =: center
      margin-top      =: 8px
      margin-bottom   =: 8px

data LearnPure
instance Theme LearnPure where
  theme c = 
    is c do
      callToActionButtonStyles (toTxt green { brightness = 80 }) (toTxt base)

data GetPure
instance Theme GetPure where
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

