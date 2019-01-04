module Shared.Styles where

import Pure
import Pure.Data.CSS
import Pure.Theme

import Control.Monad

pageStyles = do
  is "html" .> do
    boxSizing  =: borderBox
    height =: per 100

  is "body" .> do
    height =: per 100
    margin =: zero
    fontFamily =: "-apple-system, system-ui, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', sans-serif;"
    "text-rendering" =: "optimizeLegibility"
    "-webkit-font-smoothing" =: antialiased
    "-moz-osx-font-smoothing" =: "grayscale"

data PageT = PageT
instance Themeable PageT where
  theme c _ = void $ pageStyles

