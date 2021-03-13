module Tutorials (author,version,global) where

import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.Author as Author
import qualified Components.Published as Published
import qualified Components.Markdown as Markdown
import qualified Components.More as More
import qualified Components.Problem as Problem
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import qualified Components.Searcher as Searcher
import qualified Components.Subtitle as Subtitle
import Data.Route 
import Styles.Themes

import Shared
import Shared.Tutorial
import Shared.Types as Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producing, producingKeyed)

import qualified Data.List as List
import Data.Maybe (isNothing)
import GHC.Exts (IsList(..))

author :: App.App => Name -> View
author a = producingKeyed a producer (\a -> consuming (tutorials (AuthorRoute (AuthorTutorialsR a))))
  where producer = App.req backend listAuthorTutorials

version :: App.App => PackageName -> Types.Version -> View
version pn v = producingKeyed (pn,v) producer (\(pn,v) -> consuming (tutorials (PackageRoute (PackageTutorialR pn v TutorialsR))))
  where producer = App.req backend listPackageVersionTutorials

global :: App.App => View
global = producing producer (consuming (tutorials (TutorialRoute TutorialsR)))
  where producer = App.req backend listTutorials ()

tutorials :: App.App => Route -> [Tutorial Rendered] -> View
tutorials _ [] = Problem.emptyList "No tutorials, yet." Null
tutorials rt ts = Searcher.searcher listing ts
  where
    listing v search xs
      | ts <- List.filter (\t -> isNothing (episode (t :: Tutorial Rendered))) xs
      = Div <| Themed @Searcher . Themed @Load |>
        [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Tutorials"
        , Div <||> 
          [ Div <| Themed @Listing |>
            [ Article <| Themed @Article |> 
              [ Header <| Themed @Header |> 
                [ Avatar.avatars (toList authors)
                , Title.title t (toTxt title)
                , Subtitle.subtitle subtitle 
                , Author.authors authors
                , Published.published published
                , Tags.searchableTags tags search
                ]
              , Markdown.markdown e
              , Markdown.markdown $ Rendered
                [ Div <| Class "hide" 
                , More.more t
                ]
              ]
            ]
          | Tutorial {..} <- xs 
          , let t = toSpecificTutorial rt slug
          , let Excerpt e = excerpt
          ]
        ]     