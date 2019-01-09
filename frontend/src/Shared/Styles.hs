module Shared.Styles where

import Pure
import Pure.Data.CSS
import Pure.Theme

import Control.Monad

import Shared.Colors

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

data PageT = PageT
instance Themeable PageT where
  theme c _ = void $ do
    pageStyles

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

      atMedia "(max-width: 48em)" .> do
        marginRight =: pxs (-16)

      has "h2" .> do
        margin =: pxs 8
        fontSize =: pxs 20

      has "p" .> do
        marginTop =: zero

    has "pre" . is ".sourceCode" .> do
      marginTop        =: pxs 40
      marginLeft       =: pxs (-16)
      marginRight      =: pxs (-16)
      marginBottom     =: pxs 16
      fontSize         =: ems 1
      fontFamily       =: "source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
      fontWeight       =: int 300
      "-webkit-font-smoothing" =: auto
      backgroundColor  =: bg
      color            =: fg
      overflow         =: auto

    has "pre" . is ".sourceCode" $ do
      atMedia "(min-width: 48em)" .> do
        borderRadius     =: pxs 10
        marginLeft       =: pxs 16
        marginRight      =: pxs 16

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
