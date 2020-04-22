module Styles.Themes (module Styles.Themes, module Export, pattern Theme, Themeable(..)) where

import Styles.Colors
import Styles.Fonts
import Styles.Responsive

import Pure.Elm
import Pure.Data.CSS as Export
import Pure.Theme

import Control.Monad as Export

import Prelude hiding (or)

fontStyle :: Txt
fontStyle = "font-style"

data AppT = AppT
instance Themeable AppT where
  theme c _ = void $ do
    is c . child "div" .> do
      height =: per 100
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
    is "*" . or is ":after" . or is ":before" .> do
      boxSizing  =: inherit
      "-wekit-box-sizing" =: inherit

    is "html" .> do
      boxSizing =: borderBox

    is "body" .> do
      fontFamily =: defaultFont
      "text-rendering" =: "optimizeLegibility"
      "-webkit-font-smoothing" =: antialiased
      "-moz-osx-font-smoothing" =: "grayscale"

    -- page defaults

    is "html" .> do
      height =: per 100

    is "body" .> do
      margin =: zero
      height =: per 100

    is "body" . child "div" .> do
      width  =: per 100
      height =: per 100

data MarkdownT = MarkdownT
instance Themeable MarkdownT where
  theme c _ = void $ is c $ do
    id .> do
      width =: per 100

    atMedia "(max-width: 480px)" .> do
      fontSize =: ems 0.45

    atMedia "(min-width: 481px)" .> do
      fontSize =: ems 0.85

    has "table" $ do

      apply $ do
        color =: "#666"
        fontSize =: pxs 12
        textShadow =: "1px 1px 0px #fff"
        background =: "#eaebec"
        border =: "#ccc 1px solid"
        "-moz-border-radius" =: pxs 3
        "-webkit-border-radius" =: pxs 3
        borderRadius =: pxs 3
        "-moz-box-shadow" =: "0 1px 2px #d1d1d1"
        "-webkit-box-shadow" =: "0 1px 2px #d1d1d1"
        boxShadow =: "0 1px 2px #d1d1d1"
        tableLayout =: fixed
        width =: per 100

      has "a" $ do

        is ":link" .> do
          color =: "#666"
          fontWeight =: bold
          textDecoration =: none

        is ":active" .> do
          color =: "#bd5a35"
          textDecoration =: underline

        is ":hover" .> do
          color =: "#bd5a35"
          textDecoration =: underline

      has "th" $ do

        apply $ do
          overflowX =: scroll
          padding =: "8px 12px 8px 12px"
          borderTop =: "1px solid #fafafa"
          borderBottom =: "1px solid #e0e0e0"
          background =: "#ededed"
          background =: "-webkit-gradient(linear, left top, left bottom, from(#ededed), to(#ebebeb))"
          background =: "-moz-linear-gradient(top,  #ededed,  #ebebeb)"
        is ":first-child" .> do
          textAlign =: left
          paddingLeft =: pxs 20

      has "tr" $ do

        apply $ do
          textAlign =: center
          paddingLeft =: pxs 20

        is ":first-child" . has "th" $ do

          is ":first-child" .> do
            "-moz-border-radius-topleft" =: pxs 3
            "-webkit-border-top-left-radius" =: pxs 3
            "border-top-left-radius" =: pxs 3

          is ":last-child" .> do
            "-moz-border-radius-topright" =: pxs 3
            "-webkit-border-top-right-radius" =: pxs 3
            "border-top-right-radius" =: pxs 3

        is ":nth-child(even)" .> do
          background =: "#f6f6f6"
          background =: "-webkit-gradient(linear, left top, left bottom, from(#f8f8f8), to(#f6f6f6))"
          background =: "-moz-linear-gradient(top,  #f8f8f8,  #f6f6f6)"

        is ":last-child" . has "td" $ do
          apply $ borderBottom =: zero

          is ":first-child" .> do
            "-moz-border-radius-bottomleft" =: pxs 3
            "-webkit-border-bottom-left-radius" =: pxs 3
            "border-bottom-left-radius" =: pxs 3

          is ":last-child" .> do
            "-moz-border-radius-bottomright" =: pxs 3
            "-webkit-border-bottom-right-radius" =: pxs 3
            "border-bottom-right-radius" =: pxs 3

        is ":hover" . has "td" .> do
          background =: "#f2f2f2"
          background =: "-webkit-gradient(linear, left top, left bottom, from(#f2f2f2), to(#f0f0f0))"
          background =: "-moz-linear-gradient(top,  #f2f2f2,  #f0f0f0)"


      has "td" $ do

        apply $ do
          padding =: pxs 18
          borderTop =: "1px solid #ffffff"
          borderBottom =: "1px solid #e0e0e0"
          borderLeft =: "1px solid #e0e0e0"
          background =: "#fafafa"
          background =: "-webkit-gradient(linear, left top, left bottom, from(#fbfbfb), to(#fafafa))"
          background =: "-moz-linear-gradient(top,  #fbfbfb,  #fafafa)"

        is ":first-child" .> do
          wordBreak =: breakWord
          textAlign =: center
          borderLeft =: zero

    has "p" .> do
      lineHeight =: ems 1.7
      fontSize   =: pxs 18
      fontWeight =: int 300
      color      =: darkGray
      margin     =: pxs 16

    let greenHighlight = do
          background       =: pureGreen 85
          borderBottom     =: pxs 1 <<>> solid <<>> baseGreen
          color            =: selection
          textDecoration   =: none

    has "p" . has "a" $ do
      apply greenHighlight

      is hovered .> do
        background       =: lightGreen
        borderBottom     =: pxs 1 <<>> solid <<>> darkGreen

    has "ul" .> do
      listStyle =: none

    has "h1" .> do
      fontFamily  =: defaultFont
      fontSize    =: pxs 48
      margin      =: zero

    has "h2" .> do
      fontSize   =: pxs 32

    has "h3" .> do
      fontSize   =: pxs 28
      paddingTop =: ems 1
      borderTop  =: solid <<>> pxs 1 <<>> lightGray

    has "h3" . has "a" $ do

      apply greenHighlight

      is hovered .> do
        background       =: lightGreen
        borderBottom     =: pxs 1 <<>> solid <<>> darkGreen


    has "p" . has "code" .> do
      fontFamily       =: defaultMonoFont
      wordBreak        =: breakWord
      padding          =: zero <<>> pxs 4
      background       =: pureOrange 88

    has "blockquote" $ do
      apply $ do
        backgroundColor =: pureOrange 93
        borderLeft      =: pxs 10 <<>> solid <<>> pureRed 82
        padding         =: pxs 16 <<>> pxs 0 <<>> pxs 16 <<>> pxs 16
        margin          =: pxs 20 <<>> pxs 0 <<>> pxs 20 <<>> pxs (-16)

      atMedia "(max-width: 48em)" $ do
        apply $
          marginRight =: pxs (-16)

        has ".sourceCode" . has "pre" .> do
          marginRight =: zero

      atMedia "(min-width: 48em)" .> do
        marginLeft  =: pxs 16
        marginRight =: pxs 16

      has "h2" .> do
        margin =: pxs 8
        fontSize =: pxs 20

      has "p" .> do
        marginTop =: zero

    has "pre" . is ".sourceCode" .> do
      marginLeft       =: pxs (-16)
      marginRight      =: pxs (-16)
      fontSize         =: ems 1
      fontFamily       =: defaultMonoFont
      fontWeight       =: int 300
      "-webkit-font-smoothing" =: auto
      backgroundColor  =: black
      color            =: mono1
      overflow         =: auto

    has "pre" . is ".sourceCode" $
      atMedia "(min-width: 48em)" .> do
        borderRadius     =: pxs 10
        marginLeft       =: pxs 16
        marginRight      =: pxs 16

    has "code" . is ".sourceCode" .> do
      fontFamily       =: inherit
      margin           =: pxs 16
      lineHeight       =: ems 1.3
      display          =: block
      paddingBottom    =: pxs 2

    has ".sourceLine" .> do
      whiteSpace       =: preWrap
      display          =: inlineBlock
      lineHeight       =: ems 1.5
      width            =: per 100

    has "code" . has "span" $ do
      is ".co" .> do { color =: mono3; fontStyle =: italic }                         -- Comment
      is ".dt" .> color =: orange2                                                   -- DataType
      is ".kw" .> do { color =: purple_ }                                            -- Keyword
      is ".cf" .> do { color =: purple_ }                                            -- ControlFlow
      is ".op" .> color =: mono1                                                     -- Operator
      is ".ot" .> color =: blue_                                                     -- Other
      is ".sc" .> color =: blue_                                                     -- SpecialChar
      is ".ss" .> color =: blue_                                                     -- SpecialString
      is ".vs" .> color =: blue_                                                     -- VerbatimString
      is ".cn" .> color =: orange1                                                   -- Constant
      is ".dv" .> color =: orange1                                                   -- DecVal
      is ".bn" .> color =: orange1                                                   -- BaseN
      is ".fl" .> color =: orange1                                                   -- Float
      is ".ch" .> color =: orange1                                                   -- Char
      is ".st" .> color =: green_                                                    -- String
      is ".va" .> color =: red1                                                      -- Variable
      is ".fu" .> color =: cyan_                                                     -- Function
      is ".al" .> do { color =: red2; fontWeight =: bold }                           -- Alert
      is ".er" .> do { color =: red2; fontWeight =: bold }                           -- Error
      is ".wa" .> do { color =: red1; fontWeight =: bold; fontStyle =: italic }      -- Warning
      is ".im" .> color =: purple_                                                   -- Import
      is ".bu" .> color =: purple_                                                   -- BuiltIn
      is ".ex" .> color =: purple_                                                   -- Extension
      is ".do" .> do { color =: mono3; fontStyle =: italic }                         -- Documentation
      is ".an" .> do { color =: purple_; fontWeight =: bold; fontStyle =: italic }   -- Annotation
      is ".cv" .> do { color =: mono3; fontWeight =: bold; fontStyle =: italic }     -- CommentVar
      is ".in" .> do { color =: mono3; fontWeight =: bold; fontStyle =: italic }     -- Information

data PageT = PageT
instance Themeable PageT where
  theme c _ = void $ 
    is c $ do
      apply $ do
        -- always at least 100% height
        minHeight     =: per 100

        background    =: baseWhite

      child "*" .> do
        -- always at least 100% height 
        minHeight    =: per 100

        -- up to 1200px width
        width        =: per 100
        maxWidth     =: pxs 1200

        -- pad left and right
        paddingLeft  =: pxs 16
        paddingRight =: pxs 16

        -- center content
        marginLeft   =: auto
        marginRight  =: auto

data WithHeaderT = WithHeaderT
instance Themeable WithHeaderT where
  theme c _ = void $
    is c $ 
      apply $ do
        -- push-down from header
        marginTop    =: pxs 64 

data WithSidebarT = WithSidebarT
instance Themeable WithSidebarT where
  theme c _ = void $
    is c $ do
      apply $ do
        display    =: flex
        flexWrap   =: wrap
        minHeight  =: per 100

      child "*" . is ":last-child" .> do
        margin =: pxs 10
        flexBasis =: pxs 268
        flexGrow =: one

      child "*" . is ":first-child" .> do
        flexBasis =: zero
        flexGrow  =: int 999
        minWidth  =: "calc(" <> per 75 <<->> pxs 20 <> ")"

data WithoutSidebarT = WithoutSidebarT
instance Themeable WithoutSidebarT where
  theme c _ = void $ is c $
    is c $ do
      apply $ do
        display    =: flex
        flexWrap   =: wrap

      child "*" .> do
        flexBasis =: zero
        minWidth =: per 100

data UnhideT = UnhideT
instance Themeable UnhideT where
  theme c _ = void $ 
    is c $
      has ".hide" $ do
        apply $ display =: none
        next "*" .>
          display =: none

data HiddenMediumT = HiddenMediumT
instance Themeable HiddenMediumT where
  theme c _ = void $ 
    is c $ do
      apply $ 
        display =: none

      largeScreens <#> do
        display =: block

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()

pattern Page :: View -> View
pattern Page content = 
  Children [ content ] (Theme PageT Div)

pattern WithHeader :: View -> View -> View
pattern WithHeader header content =
  Children [ header, content ] (Theme WithHeaderT Div)

pattern WithSidebar :: View -> View -> View
pattern WithSidebar sidebar content =
  Children [ content , sidebar ] (Theme WithSidebarT Div)

pattern WithoutSidebar :: View -> View
pattern WithoutSidebar content =
  Children [ content ] (Theme WithoutSidebarT Div)

pattern HiddenMedium :: View -> View
pattern HiddenMedium content =
  Children [ content ] (Theme HiddenMediumT Div)

