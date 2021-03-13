module Components.More (more,More,Unhide) where

import qualified Components.Preload as Preload
import Data.Route

import Pure.Elm.Application 

more :: Route -> View
more rt = 
  Div <| Themed @More |> 
    [ A <| Preload.prelink rt |> 
      [ "Read More >" ]
    ]

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