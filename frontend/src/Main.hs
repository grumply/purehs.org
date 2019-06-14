module Main where

import Pure.Elm

import Client
import Router
import Startup
import Types
import Update
import View

main = inject body (run routed)
  where
    routed :: Routed Model Msg
    routed = Routed router app

    app :: App Model Msg
    app = App startup model update view
