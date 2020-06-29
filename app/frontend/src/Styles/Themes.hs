{-# language TypeApplications #-}
module Styles.Themes where

import Styles.Colors
import Styles.Fonts
import Styles.Responsive

import Pure.Elm hiding (selection,green,lavender,black,green,brightness,gray)
import Pure.Spinners

import Prelude hiding (or,max,min,reverse)

data AppT
instance Theme AppT where
  theme c = void $ do
    is c . child (tag Div) .> do
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
    is "*" . or is after . or is before .> do
      box-sizing  =: inherit
      webkit-box-sizing =: inherit

    is (tag Html) .> do
      box-sizing =: border-box
      background-color =: toTxt base

    is "body" .> do
      font-family =: defaultFont
      text-rendering =: "optimizeLegibility"
      webkit-font-smoothing =: antialiased
      moz-osx-font-smoothing =: grayscale

    -- page defaults

    is (tag Html) .> do
      height =: (100%)

    is "body" .> do
      margin =: 0
      height =: (100%)

    is "body" . child (tag Div) .> do
      width  =: (100%)
      height =: (100%)

    is (tag A) .> do
      text-decoration =: none

data PageT
instance Theme PageT where
  theme c = void $ 
    is c $ do
      apply $ do
        height =: (100%)

      child "*" .> do
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
  theme c = void $ do
    is c $ do
      apply $ do 
        padding-top =: 40px

      mediumScreens <%> do
        padding-top =: 64px

      child (tag Div) . child (tag Div) .> do
        padding-bottom =: 1px
    

data HideT
instance Theme HideT where
  theme c = void $
    is c $ do
      has (subtheme @MoreT) .> 
        display =: none

      has ".hide" $ do
        apply $ 
          display =: none

        -- display any .more elements iff there is a .hide 
        -- element before it at the same level
        nexts (subtheme @MoreT) .> do
          display =: initial

data UnhideT
instance Theme UnhideT where
  theme c = void $ do
    is c . is c $ do
      has ".hide" $ do
        apply $ 
          display =: initial

        nexts (subtheme @MoreT) .> do
          display =: none

data MoreT
instance Theme MoreT where
  theme c = void $
    is c $ do

      has (tag A) $ do
        apply $ do
          display       =: block
          text-align    =: right
          margin-right  =: 16px
          margin-top    =: 30px
          color         =: toTxt black
          border-bottom =: none
          background    =: none
          font-size     =: 18px

        is hover .> do
          color =: toTxt green
          background =: none

        is visited .> do
          color =: toTxt black
          background =: none

        is visited . is hover .> do
          color =: toTxt green
          background =: none

data HiddenMediumT
instance Theme HiddenMediumT where
  theme c = void $ 
    is c $ do
      apply $ 
        display =: none

      largeScreens <%> do
        display =: block

data LoadingT
instance Theme LoadingT where
  theme c = void $ is c $ return ()

data FailedT
instance Theme FailedT where
  theme c = void $ is c $ return ()

page c = Div <| Themed @PageT |> [ c ]

withHeader h c = 
  Div <| Themed @WithHeaderT |> 
    [ h , c ]

data ErrorT
instance Theme ErrorT where
  theme c = void $ 
    is c $ do
      apply $ do
        width  =: (90%)
        margin =* [40px,auto]

      mediumScreens <%> do
        width =: 720px

      largeScreens <%> do
        width =: 900px

      child (tag Header) $ do
        child (tag H1) $ do
          apply $ do
            margin-top    =: 0
            margin-bottom =: 24px
            color         =: toTxt lavender
            font-size     =: 4em
            font-family   =: titleFont
        
        child (tag H2) .> do
          margin-bottom =: 44px
          color         =: toTxt black
          font-size     =: 1.5em
          font-family   =: titleFont

      has (tag P) $ do
        apply $ do
          color =: toTxt black
          font-family =: defaultFont
          line-height =: 28px
          font-size   =: 16px

        has (tag A) $ do

          apply $ do
            background-color =: toTxt (faded green)
            border-bottom    =* [1px,solid,toTxt green]
            color            =: toTxt black
            text-decoration  =: none

          is hover .> do
            background    =: toTxt green
            border-bottom =* [1px,solid,toTxt black]

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

notFound nm = problems nm $ Div <||>
  [ P <||> [ "Please contact the owner of the site that linked you to the original URL and let them know their link is broken." ]
  , P <||> 
    [ "If an internal link brought you here, please file a bug report "
    , A <| Rel "noopener" . Attribute "target" "_blank" . Href "https://github.com/grumply/purehs.org/issues/new" |> [ "here" ]
    , "."
    ]
  ]

emptyList h1 h2 = 
  Div <||>
    [ Div <| Themed @ErrorT |>
      [ Header <||> 
        [ H1 <||> [ h1 ]
        , H2 <||> [ h2 ]
        ]
      ]
    ]


loading = 
    View (ChasingDots :: ChasingDots 2000 40 40 40 "#333")

data ButtonT
instance Theme ButtonT where
  theme c = void $ is c .> do
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
  theme c = void $ do
    is (subtheme @PlaceholderT) . has c $ do
      has (tag A) .> do
        pointer-events =: none
        filter_ =: blur(8px)

    is c . has (subtheme @VersionT) $ do
      apply $ 
        color =: toTxt gray

      is (subtheme @LatestT) .> 
          color =: toTxt black
   
      is hover $ do
        apply $ 
          color =: toTxt green

        is ":visited" $ do
          apply $
            color =: toTxt green

          is (subtheme @LatestT) .> 
            color =: toTxt green

      is ":visited" $ do
        apply $
          color =: toTxt gray

        is (subtheme @LatestT) .> 
          color =: toTxt black


data HeaderT
instance Theme HeaderT where
  theme c = void $ do
    is (subtheme @PlaceholderT) . has c .> do
      pointer-events =: none
      filter_        =: blur(12px)

    is c $ do
      apply $ do
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
  theme c = void $ do
    is (subtheme @PlaceholderT) . has c .> do
      pointer-events =: none
      filter_ =: blur(10px)

    is c .> do
      font-family =: titleFont
      font-size =: 1.2em
      font-weight =: 400
      color =: toTxt base

buttonBoxShadow vOff blur color vOff' blur' color' =
  customBoxShadow 0 vOff blur 0 color <> ", " <>
  customBoxShadow 0 vOff' blur' 0 color'

customBoxShadow hOff vOff blur spread color = 
  pxs hOff <<>> pxs vOff <<>> pxs blur <<>> pxs spread <<>> color

data PageHeaderT
instance Theme PageHeaderT where
  theme c = void $ 
    is c $ do
      apply $ do
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

      has (tag Nav) $ do
        apply $ do
          margin-top  =: 30px
          color       =: toTxt black
          font-weight =: 400
          width       =: (100%)

        is lastChild $ do
          apply $ do
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


        has (tag A) $ do
          apply $ do
            margin-top   =: 50px
            font-family  =: titleFont
            margin-right =: 5px
            margin-left  =: 5px
            color        =: toTxt gray
            white-space  =: nowrap

          is firstChild .> do
            margin-left =: 0

          is lastChild .> do
            margin-right =: 0

          is hover .>
            color =: toTxt green

          is hover . is visited .>
            color =: toTxt green

          is visited .>
            color =: toTxt gray
    
