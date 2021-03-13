module Packages where

import App (req,App)
import qualified Components.Author as Author
import qualified Components.Description as Description
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import Components.Searcher ( searcher )
import Data.Route
import Styles.Themes

import Shared
import Shared.Package
import Shared.Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producing, producingKeyed)

global :: App.App => View
global = producing producer (consuming packages)
  where producer = App.req backend listPackages ()

author :: App.App => Name -> View
author a = producingKeyed a producer (\_ -> consuming packages)
  where producer = App.req backend listAuthorPackages

packages :: App.App => [Package] -> View
packages = searcher listing
  where
    listing v search xs =
      Div <| Themed @Searcher |>
        [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Packages"
        , Div <||> 
          [ Div <| Themed @Listing . Themed @Divided . Themed @Load |>
            [ Div <||> -- flex space between
              [ Div <||>
                [ Author.author author
                , " ❯ " 
                , Title.title (PackageRoute (PackageR name)) (toTxt name)
                ]
              , Title.title (PackageRoute (PackageVersionR name latest)) (toTxt latest)
              ]
            , Tags.searchableTags tags search
            , Description.description description
            ]
          | Package {..} <- xs 
          ]
        ]

