module Components.Author (authors, author, Author(), Authors()) where

import Components.Preload ( prelink )
import Data.Route

import Shared.Types (Authors(..),Name)

import Styles.Colors ( lavender, green )
import Styles.Fonts ( titleFont )
import Styles.Themes ( Listing )

import Pure.Elm.Application hiding (render,Title,green,black,lavender,brightness,blue)

import Data.List as List ( intersperse )

authors :: Authors -> View
authors (Authors as) =
  Div <| Themed @Authors |>
    List.intersperse ", " 
      [ author a
      | a <- as 
      ]

author :: Name -> View
author nm =
    Address <| Themed @Author |>
      [ A <| Rel "author" . prelink (AuthorRoute (AuthorR nm)) |>
        [ txt nm ]
      ]

instance Theme Authors where
  theme c =
    is c do
      display =: inline

data Author
instance Theme Author where
  theme c =
    is c do
      display =: inline

      has (tag A) do
        font-family =: titleFont
        font-size   =: 16px
        font-style  =: normal
        color       =: toTxt lavender
         
        hover do
          color =: toTxt green

      within @Listing do
        has (tag A) do
          font-size =: 20px

        has (tag A) do
          font-size =: 22px

