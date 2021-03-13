module Blog (global,author,package) where

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
import Shared.Blog
import Shared.Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producing, producingKeyed)

import Data.Maybe (isNothing)
import qualified Data.List as List
import GHC.Exts (IsList(..))

global :: App.App => View
global = producing producer (consuming (blog (BlogRoute BlogR)))
  where producer = App.req backend listPosts ()

author :: App.App => Name -> View 
author a = producingKeyed a producer (\a -> consuming (blog (AuthorRoute (AuthorBlogR a))))
  where producer = App.req backend listAuthorPosts

package :: App.App => PackageName -> View
package pn = producingKeyed pn producer (\pn -> consuming (blog (PackageRoute (PackageBlogR pn BlogR))))
  where producer = App.req backend listPackagePosts

blog :: App.App => Route -> [Post Rendered] -> View
blog _ [] = Problem.emptyList "No posts, yet." Null
blog rt xs = Searcher.searcher listing xs
  where
    listing v search xs
      | ts <- List.filter (\t -> isNothing (episode (t :: Post Rendered))) xs
      = Div <| Themed @Searcher . Themed @Load |>
        [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Posts"
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
          | Post {..} <- xs 
          , let t = toSpecificPost rt slug
          , let Excerpt e = excerpt
          ]
        ]     

