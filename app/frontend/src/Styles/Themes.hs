{-# language TypeApplications #-}
module Styles.Themes where

import Styles.Colors
import Styles.Fonts
import Styles.Responsive

import Pure.Elm hiding (App,selection,green,lavender,black,green,brightness,gray)
import Pure.Spinners

import Prelude hiding (or,max,min,reverse,any)

data App
instance Theme App where
  theme c = do
    is c do 
      child (tag Div) do
        height =: (100%)

    -- opinionated resets
    -- system fonts chosen under the assumption that they are well-optimized.
    -- border-box is, currently, the only sane box model.
    --
    -- Page styles:
    --  Set font default to system fonts
    --  Optimize fonts for clean look and feel
    --  Set box model to border-box
    --  Set a default 100% height
    --
    using [any,after,before] do
      box-sizing  =: inherit
      webkit-box-sizing =: inherit

    is (tag Html) do
      box-sizing =: border-box
      background-color =: toTxt base

    is "body" do
      font-family =: defaultFont
      text-rendering =: "optimizeLegibility"
      webkit-font-smoothing =: antialiased
      moz-osx-font-smoothing =: grayscale

    -- page defaults

    is (tag Html) do
      height =: (100%)

    is "body" do
      margin =: 0
      height =: (100%)

    is "body" do
      child (tag Div) do
        width  =: (100%)
        height =: (100%)

    is (tag A) do
      text-decoration =: none

data Page
instance Theme Page where
  theme c =
    is c do
      height =: (100%)

      child "*" do
        height         =: (100%)
        width          =: (100%)
        max-width      =: 1200px
        padding-left   =: 16px
        padding-right  =: 16px
        margin-left    =: auto
        margin-right   =: auto
        padding-bottom =: 10px

data WithHeader
instance Theme WithHeader where
  theme c =
    is c do
      padding-top =: 40px

      mediumScreens <%> do
        padding-top =: 64px

      child (tag Div) do
        child (tag Div) do
          padding-bottom =: 1px
    

data Hide
instance Theme Hide where
  theme c =
    is c do
      has (subtheme @More) do
        display =: none

      has ".hide" do
        display =: none

        -- display any .more elements iff there is a .hide 
        -- element before it at the same level
        nexts (subtheme @More) do
          display =: initial

data Unhide
instance Theme Unhide where
  theme c =
    is c do
      -- double up for precedence
      is c do
        has ".hide" do
          display =: initial

          nexts (subtheme @More) do
            display =: none

data More
instance Theme More where
  theme c =
    is c do

      has (tag A) do
        display       =: block
        text-align    =: right
        margin-right  =: 16px
        margin-top    =: 30px
        color         =: toTxt black
        border-bottom =: none
        background    =: none
        font-size     =: 18px

        hover do
          color =: toTxt green
          background =: none

        visited do
          color =: toTxt black
          background =: none

        visited do
          hover do
            color =: toTxt green
            background =: none

data HiddenMedium
instance Theme HiddenMedium where
  theme c =
    is c do
      display =: none

      largeScreens <%> do
        display =: block

data Loading
instance Theme Loading 

data Failed
instance Theme Failed 

page :: View -> View
page c = Div <| Themed @Page |> [ c ]

withHeader :: View -> View -> View
withHeader h c = 
  Div <| Themed @WithHeader |> 
    [ h , c ]

data Error
instance Theme Error where
  theme c =
    is c do
      width  =: (90%)
      margin =* [40px,auto]

      mediumScreens <%> do
        width =: 720px

      largeScreens <%> do
        width =: 900px

      child (tag Header) do
        child (tag H1) do
          margin-top    =: 0
          margin-bottom =: 24px
          color         =: toTxt lavender
          font-size     =: 4em
          font-family   =: titleFont
        
        child (tag H2) do
          margin-bottom =: 44px
          color         =: toTxt black
          font-size     =: 1.5em
          font-family   =: titleFont

      has (tag P) do
        color =: toTxt black
        font-family =: defaultFont
        line-height =: 28px
        font-size   =: 16px

        has (tag A) do

          background-color =: toTxt (faded green)
          border-bottom    =* [1px,solid,toTxt green]
          color            =: toTxt black
          text-decoration  =: none

          hover do
            background    =: toTxt green
            border-bottom =* [1px,solid,toTxt black]

problems :: Txt -> View -> View
problems nm c =
    Div <||>
      [ Div <| Themed @Error |>
        [ Header <||>
          [ H1 <||> [ fromTxt (nm <> " not found.") ]
          , H2 <||> [ "We couldn't find what you were looking for." ]
          ]
        , c
        ]
      ]

notFound :: Txt -> View
notFound nm = problems nm $ Div <||>
  [ P <||> [ "Please contact the owner of the site that linked you to the original URL and let them know their link is broken." ]
  , P <||> 
    [ "If an internal link brought you here, please file a bug report "
    , A <| Rel "noopener" . Attribute "target" "_blank" . Href "https://github.com/grumply/purehs.org/issues/new" |> [ "here" ]
    , "."
    ]
  ]

emptyList :: View -> View -> View
emptyList h1 h2 = 
  Div <||>
    [ Div <| Themed @Error |>
      [ Header <||> 
        [ H1 <||> [ h1 ]
        , H2 <||> [ h2 ]
        ]
      ]
    ]


loading :: View
loading = 
    View (ChasingDots :: ChasingDots 2000 40 40 40 "#333")

data Button
instance Theme Button where
  theme c = is c do
    display          =: inline-block
    margin           =* [0px,16px]
    height           =: 44px
    line-height      =: 44px
    padding          =* [0,14px]
    box-shadow       =: buttonBoxShadow 4 6 (toTxt lavender) 1 3 (rgba(0,0,0,0.13))
    border-radius    =: 5px
    font-weight      =: 600
    text-transform   =: uppercase
    letter-spacing   =: 0.025em
    text-decoration  =: none
    white-space      =: nowrap

data Placeholder
instance Theme Placeholder where
  theme c =
    is c do
      important do
        pointer-events =: none
        filter_        =: blur(12px)

data Load
instance Theme Load where
  theme c = do
    is c do
      animation =* ["focus",300ms]

    atKeyframes "focus" do
      has from do
        opacity =: 0
        filter_ =: blur(8px)
      has to do
        filter_ =: none
        opacity =: 1

data Latest
instance Theme Latest
data Version
instance Theme Version
data Versions
instance Theme Versions where
  theme c =
    is c do
      has (subtheme @Version) do
        color =: toTxt gray

        at @Latest do 
          color =: toTxt black
     
        hover do
          color =: toTxt green

          visited do
            color =: toTxt green

            at @Latest do
              color =: toTxt green

        visited do
          color =: toTxt gray

          at @Latest do 
            color =: toTxt black

data Header
instance Theme Header where
  theme c =
    is c do
      text-align =: center
      width      =: (100%)
      max-width  =: (100%)
      margin     =* [0,auto,40px]

      mediumScreens <%> do
        width =: 700px

      largeScreens <%> do
        width =: 800px

      hugeScreens <%> do
        width =: 900px

data SectionTitleT
instance Theme SectionTitleT where
  theme c =
    is c do
      font-family =: titleFont
      font-size =: 1.2em
      font-weight =: 400
      color =: toTxt base

buttonBoxShadow :: Int -> Int -> Txt -> Int -> Int -> Txt -> Txt
buttonBoxShadow vOff blr clr vOff' blr' clr' =
  customBoxShadow 0 vOff blr 0 clr <> ", " <>
  customBoxShadow 0 vOff' blr' 0 clr'

customBoxShadow :: Int -> Int -> Int -> Int -> Txt -> Txt
customBoxShadow hOff vOff blr spread clr = 
  pxs hOff <<>> pxs vOff <<>> pxs blr <<>> pxs spread <<>> clr

data PageHeader
instance Theme PageHeader where
  theme c =
    is c do
      font-size       =: 18px
      width           =: (100%)
      margin          =* [0,auto]
      height          =: 80px
      margin-bottom   =: 30px

      smallScreens <%> do
        display         =: flex
        justify-content =: space-between
        font-size       =: 24px
        margin-bottom   =: 45px

      mediumScreens <%> do
        width =: 720px

      largeScreens <%> do
        width =: 900px

      has (tag Nav) do
        margin-top  =: 30px
        color       =: toTxt black
        font-weight =: 400
        width       =: (100%)

        lastChild do
          font-size =: 24px
          display =: flex
          justify-content =: space-around
          margin-right =: 16px
          margin-top =: (-30)px

          smallScreens <%> do
            display =: block
            margin-right =: 0
            width =: auto
            margin-top =: 30px

        has (tag A) do
          margin-top   =: 50px
          font-family  =: titleFont
          margin-right =: 5px
          margin-left  =: 5px
          color        =: toTxt gray
          white-space  =: nowrap

          firstChild do
            margin-left =: 0

          lastChild do
            margin-right =: 0

          hover do
            color =: toTxt green

            visited do
              color =: toTxt green

          visited do
            color =: toTxt gray

data Article
instance Theme Article where
  theme c =
    is c do
      width       =: (100%)
      padding     =* [60px,30px]
      padding-top =: 110px
      margin      =* [0,auto,100px]
      background  =: hex 0xfff
      border      =* [1px,solid,hex 0xeaecee]
      position    =: relative
      width       =: (100%)
      max-width   =: (100%)

      mediumScreens <%> do
        width =: 700px

      largeScreens <%> do
        width =: 800px

      hugeScreens <%> do
        width =: 900px

      has ".drop" do
        firstOfType do 
          firstLetter do
            font-family  =: serifFont
            font-size    =: 4em
            font-weight  =: 400
            margin-top   =: 0.2em
            margin-right =: 0.15em
            float        =: left
            color        =: toTxt lavender

      next (subtheme @Subarticles) do
        margin-top =: (-70)px

data Subarticles
instance Theme Subarticles where
  theme c =
    is c do
      padding-bottom =: 30px

      child (tag H2) do
        font-family =: titleFont
        font-size   =: 2.5em
        font-weight =: 400
        color       =: toTxt base { brightness = 45 }
        text-align  =: center

      has (tag Article) do
        padding-top    =: 60px
        margin-bottom =: 50px

      has (subtheme @Article) do
        padding =* [30px,30px]

data Listing
instance Theme Listing where
  theme c = void $ 
    is c $ do
      font-size =: 18px
      margin      =* [0,auto]
      width       =: (100%)
      width       =: (100%)
      max-width   =: (100%)

      mediumScreens <%> do
        width =: 700px

      largeScreens <%> do
        width =: 800px

      hugeScreens <%> do
        width =: 900px

      child (tag Div) do
        display =: flex
        justify-content =: space-between

      child (tag P) do
        font-family   =: defaultFont
        font-size     =: 18px
        color         =: toTxt black
        margin-top    =: 8px
        margin-bottom =: 0

data Divided
instance Theme Divided where
  theme c =
    is c do
      isn't ":last-of-type" do
        border-bottom =* [1px,solid,toTxt (faded gray)]
        margin =* [15px,auto]
        padding-bottom =: 30px

      is ":last-of-type" do
        padding-bottom =: 80px

data Searcher
instance Theme Searcher where
  theme c =
    is c do
      margin-top =: (-60)px

      smallScreens <%> do
        margin-top =: 0

      has (tag Input) do
        display       =: block
        margin        =* [30px,auto]
        font-size     =: 24px
        margin-top    =: 24px
        margin-bottom =: 24px
        border-radius =: 8px
        border        =* [1px,solid,hex 0xeee]
        outline       =: none
        line-height   =: 1.2
        padding       =: 10px
        width         =: (100%)
        margin-top    =: (-10)px
        margin-bottom =: 60px

        mediumScreens <%> do
          width =: 700px

        largeScreens <%> do
          width =: 800px

        hugeScreens <%> do
          width =: 900px
