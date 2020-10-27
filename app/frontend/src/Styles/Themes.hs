{-# language TypeApplications #-}
module Styles.Themes where

import Styles.Colors
import Styles.Fonts
import Styles.Responsive

import Pure.Elm hiding (selection,green,lavender,black,green,brightness,gray)
import Pure.Spinners

import Prelude hiding (or,max,min,reverse,any)

data AppT
instance Theme AppT where
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

data PageT
instance Theme PageT where
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

data WithHeaderT
instance Theme WithHeaderT where
  theme c =
    is c do
      padding-top =: 40px

      mediumScreens <%> do
        padding-top =: 64px

      child (tag Div) do
        child (tag Div) do
          padding-bottom =: 1px
    

data HideT
instance Theme HideT where
  theme c =
    is c do
      has (subtheme @MoreT) do
        display =: none

      has ".hide" do
        display =: none

        -- display any .more elements iff there is a .hide 
        -- element before it at the same level
        nexts (subtheme @MoreT) do
          display =: initial

data UnhideT
instance Theme UnhideT where
  theme c =
    is c do
      -- double up for precedence
      is c do
        has ".hide" do
          display =: initial

          nexts (subtheme @MoreT) do
            display =: none

data MoreT
instance Theme MoreT where
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

data HiddenMediumT
instance Theme HiddenMediumT where
  theme c =
    is c do
      display =: none

      largeScreens <%> do
        display =: block

data LoadingT
instance Theme LoadingT where
  theme c = is c $ return ()

data FailedT
instance Theme FailedT where
  theme c = is c $ return ()

page :: View -> View
page c = Div <| Themed @PageT |> [ c ]

withHeader :: View -> View -> View
withHeader h c = 
  Div <| Themed @WithHeaderT |> 
    [ h , c ]

data ErrorT
instance Theme ErrorT where
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
      [ Div <| Themed @ErrorT |>
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
    [ Div <| Themed @ErrorT |>
      [ Header <||> 
        [ H1 <||> [ h1 ]
        , H2 <||> [ h2 ]
        ]
      ]
    ]


loading :: View
loading = 
    View (ChasingDots :: ChasingDots 2000 40 40 40 "#333")

data ButtonT
instance Theme ButtonT where
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

data PlaceholderT
instance Theme PlaceholderT

data LatestT
instance Theme LatestT
data VersionT
instance Theme VersionT
data VersionsT
instance Theme VersionsT where
  theme c =
    is c do
      has (subtheme @VersionT) do
        color =: toTxt gray

        at @LatestT do 
          color =: toTxt black
     
        hover do
          color =: toTxt green

          visited do
            color =: toTxt green

            at @LatestT do
              color =: toTxt green

        visited do
          color =: toTxt gray

          at @LatestT do 
            color =: toTxt black
      
      within @PlaceholderT do
        has (tag A) do
          pointer-events =: none
          filter_ =: blur(8px)

data HeaderT
instance Theme HeaderT where
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
        
      within @PlaceholderT do
        pointer-events =: none
        filter_        =: blur(12px)

data SectionTitleT
instance Theme SectionTitleT where
  theme c =
    is c do
      font-family =: titleFont
      font-size =: 1.2em
      font-weight =: 400
      color =: toTxt base

      within @PlaceholderT do
        pointer-events =: none
        filter_ =: blur(10px)

buttonBoxShadow :: Int -> Int -> Txt -> Int -> Int -> Txt -> Txt
buttonBoxShadow vOff blr clr vOff' blr' clr' =
  customBoxShadow 0 vOff blr 0 clr <> ", " <>
  customBoxShadow 0 vOff' blr' 0 clr'

customBoxShadow :: Int -> Int -> Int -> Int -> Txt -> Txt
customBoxShadow hOff vOff blr spread clr = 
  pxs hOff <<>> pxs vOff <<>> pxs blr <<>> pxs spread <<>> clr

data PageHeaderT
instance Theme PageHeaderT where
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
        font-size =: 24px
        margin-bottom =: 45px

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
    
