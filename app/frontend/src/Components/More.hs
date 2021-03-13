module Components.More (more,More) where

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

