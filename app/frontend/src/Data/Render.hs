module Data.Render where

import qualified App

import Pure.Elm

class Render a where
  render :: App.App => a -> View

