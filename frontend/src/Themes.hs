module Themes where

import Pure.Elm
import Pure.Data.CSS
import Pure.Theme

import Control.Monad

import Prelude hiding (or)

fontStyle = "font-style"

-- Page styles:
--  Set font default to system fonts
--  Optimize fonts for clean look and feel
--  Set box model to border-box
--  Set a default 100% height
pageStyles = do

  -- opinionated resets
  -- system fonts chosen under the assumption that they are well-optimized.
  -- border-box is the only sane box model.

  is "*" . or is ":after" . or is ":before" .> do
    boxSizing  =: inherit
    "-wekit-box-sizing" =: inherit

  is "html" .> do
    boxSizing =: borderBox

  is "body" .> do
    fontFamily =: "-apple-system, system-ui, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', sans-serif;"
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

data PageT = PageT
instance Themeable PageT where
  theme c _ = void $ do
    pageStyles

data ContentfulT = ContentfulT
instance Themeable ContentfulT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingBottom =: ems 3

markdownStyles = do
    apply $ do
      marginTop  =: pxs 16

    has "p" .> do
      lineHeight =: ems 1.7
      fontSize   =: pxs 18
      fontWeight =: int 300
      color      =: darkGray
      margin     =: pxs 16

    has "p" . has "a" $ do
      apply $ do
        background       =: pureGreen 85
        borderBottom     =: pxs 1 <<>> solid <<>> baseGreen
        color            =: selection
        textDecoration   =: none

      is hovered .> do
        background       =: lightGreen
        borderBottom     =: pxs 1 <<>> solid <<>> darkGreen

    has "ul" .> do
      listStyle =: none

    has "h1" .> do
      fontFamily  =: "-apple-system, system-ui, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', sans-serif;"
      fontSize    =: pxs 48

    has "h2" .> do
      fontSize   =: pxs 32

    has "h3" .> do
      fontSize   =: pxs 28

    has "p" . has "code" .> do
      fontFamily       =: "source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
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

      has "h2" .> do
        margin =: pxs 8
        fontSize =: pxs 20

      has "p" .> do
        marginTop =: zero

    has "pre" . is ".sourceCode" .> do
      marginLeft       =: pxs (-16)
      marginRight      =: pxs (-16)
      fontSize         =: ems 1
      fontFamily       =: "source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
      fontWeight       =: int 300
      "-webkit-font-smoothing" =: auto
      backgroundColor  =: black
      color            =: mono1
      overflow         =: auto

    has "pre" . is ".sourceCode" $ do
      atMedia "(min-width: 48em)" .> do
        borderRadius     =: pxs 10
        marginLeft       =: pxs 16
        marginRight      =: pxs 0

    has "code" . is ".sourceCode" $ do
      apply $ do
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

    has "code" $ do
      has "span" $ do

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

data MarkdownT = MarkdownT
instance Themeable MarkdownT where
  theme c _ = void $ is c $ do

    atMedia "(max-width: 480px)" .> do
      fontSize =: ems 0.45

    atMedia "(min-width: 481px)" .> do
      fontSize =: ems 0.85

    markdownStyles

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        paddingTop =: ems 0.5
        textDecoration =: underline
        fontSize =: pxs 60
        color =: darkGray
      atMedia "(max-width: 779px)" . is c .> do
        fontSize =: pxs 40

data MetaT = MetaT
instance Themeable MetaT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $
    is c $ do
      -- headerOffset
      apply $ do
        minHeight    =: per 100
        width        =: per 100
        maxWidth     =: pxs 1200
        paddingLeft  =: ems 1
        paddingRight =: ems 1
        marginLeft   =: auto
        marginRight  =: auto

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data ArticleT = ArticleT
instance Themeable ArticleT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 1.5
      paddingBottom =: ems 1.5

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()

headerOffset = do
    apply $ do
      marginTop =: pxs 75

    atMedia "(max-width: 48em)" .> do
      marginTop =: pxs 50

fill :: Txt
fill = "fill"

-- Colors

pureBlue p = hsl(215,35.14,p)
pureLavender p = hsl(250,49,p)
pureWhite p = hsl(210,2,p)
pureGreen p = hsl(139,68,p)

darkBlue = pureBlue 20
baseBlue = pureBlue 40
lightBlue = pureBlue 90

baseLavender = pureLavender 69
darkLavender = pureLavender 49
lightLavender = pureLavender 89
deepLavender = pureLavender 14

baseWhite = pureWhite 99
baseGray = pureWhite 70
lightGray = pureWhite 90
darkGray = pureWhite 20

baseGreen = pureGreen 45
lightGreen = pureGreen 65
darkGreen = pureGreen 25

blueHighlight = rgb(58,173,175)

pureOrange per = hsl(29,90,per)
pureRed per = hsl(6,87,per)

h = 220
s = 13
l = 18

mono1 = hsl(h,14,71)
mono2 = hsl(h,09,55)
mono3 = hsl(h,10,40)

cyan_ = hsl(187,47,55) -- hue1
blue_ = hsl(207,82,66) -- hue2
purple_ = hsl(286,60,67) -- hue3
green_ = hsl( 95,38,62) -- hue4
red1 = hsl(355,65,65) -- hue5
red2 = hsl(  5,48,51) -- hue5-2
orange1 = hsl( 29,54,61) -- hue6
orange2 = hsl( 39,67,69) -- hue6-2

selection = hsl(220,13,10)


