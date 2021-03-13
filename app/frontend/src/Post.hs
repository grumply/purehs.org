module Post (global,author,package) where

import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.Author as Author
import qualified Components.Published as Published
import qualified Components.Problem as Problem
import qualified Components.Markdown as Markdown
import qualified Components.Subtitle as Subtitle
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import Data.Route
import Styles.Themes

import Shared
import Shared.Blog
import Shared.Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producingKeyed)

import Control.Monad (when)
import Data.Maybe (isJust,isNothing)
import GHC.Exts (IsList(..))

global :: App.App => Slug -> View
global s = producingKeyed s producer (\_ -> consuming (post (BlogRoute BlogR)))
  where 
    producer s = do
      mp  <- App.req backend getPost s
      mpc <- App.req backend getPostContent s
      mps <-
        case mp of
          Just Post {..} | Just s <- series, Nothing <- episode -> do
            ts <- App.req backend listPosts ()
            let 
              inSeries Post {..} = series == Just s
              isEpisode Post {..} = isJust episode
              es = filter ((&&) <$> inSeries <*> isEpisode) ts
            pure (Just es)
          _ -> 
            pure Nothing

      when (isNothing mp) do
        retitle "Not Found"

      pure do
        p  <- mp
        pc <- mpc
        ps <- maybe (Just []) Just mps
        pure (p,pc,ps)

author :: App.App => Name -> Slug -> View
author a s = producingKeyed (a,s) producer (\(a,s) -> consuming (post (AuthorRoute (AuthorBlogR a))))
  where
    producer (a,s) = do
      mp  <- App.req backend getPost s
      mpc <- App.req backend getPostContent s
      mps <-
        case mp of
          Just Post {..} | Just s <- series, Nothing <- episode -> do
            ts <- App.req backend listAuthorPosts a
            let 
              inSeries Post {..} = series == Just s
              isEpisode Post {..} = isJust episode
              es = filter ((&&) <$> inSeries <*> isEpisode) ts
            pure (Just es)
          _ -> 
            pure Nothing

      when (isNothing mp) do
        retitle "Not Found"

      pure do
        p  <- mp
        pc <- mpc
        ps <- maybe (Just []) Just mps
        pure (p,pc,ps)

package :: App.App => PackageName -> Slug -> View
package pn s = producingKeyed (pn,s) producer (\(pn,s) -> consuming (post (PackageRoute (PackageBlogR pn (PostR s)))))
  where
    producer (pn,s) = do
      mp  <- App.req backend getPackagePost (pn,s)
      mpc <- App.req backend getPackagePostContent (pn,s)
      mps <-
        case mp of
          Just Post {..} | Just s <- series, Nothing <- episode -> do
            ts <- App.req backend listPackagePosts pn
            let 
              inSeries Post {..} = series == Just s
              isEpisode Post {..} = isJust episode
              es = filter ((&&) <$> inSeries <*> isEpisode) ts
            pure (Just es)
          _ -> 
            pure Nothing

      when (isNothing mp) do
        retitle "Not Found"

      pure do
        p  <- mp
        pc <- mpc
        ps <- maybe (Just []) Just mps
        pure (p,pc,ps)

post :: App.App => Route -> Maybe (Post Rendered,PostContent Rendered,[Post Rendered]) -> View
post rt = \case
  Nothing -> Problem.notFound "Tutorial"
  Just (Post {..},PostContent md,ps) ->
    Div <| Themed @Load |>
      [ Article <| Themed @Article |> 
        [ Header <| Themed @Header |>
          [ Avatar.avatars (toList authors)
          , Title.title rt (toTxt title)
          , Subtitle.subtitle subtitle 
          , Author.authors authors
          , Published.published published
          , Tags.tags tags 
          ]
        , Markdown.markdown md
        ]
      , case ps of
          [] -> Null
          ts ->
            Div <| Themed @Subarticles |> 
              [ H2 <||> [ "Series" ]
              , Div <||> 
                [ Article <| Themed @Article |> 
                  [ Header <| Themed @Header |>
                    [ Title.title t (toTxt title)
                    , Subtitle.subtitle subtitle 
                    , Author.authors authors
                    , Published.published published
                    , Tags.tags tags 
                    ]
                  , Markdown.markdown md
                  ]
                | Post {..} <- ts
                , let Excerpt md = excerpt
                , let t = toSpecificPost rt slug
                ]
              ]
      ]

