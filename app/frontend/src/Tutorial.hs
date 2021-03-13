module Tutorial (global,package) where

import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.Author as Author
import qualified Components.Published as Published
import qualified Components.Markdown as Markdown
import qualified Components.Problem as Problem
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import qualified Components.Subtitle as Subtitle
import Data.Route
import Styles.Themes

import Shared
import Shared.Tutorial
import Shared.Types as Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producingKeyed)

import Control.Monad (when)
import Data.Ord (compare) 
import Data.Function (on)
import qualified Data.List as List
import Data.Maybe (isJust,isNothing)
import GHC.Exts (IsList(..))

global :: App.App => Slug -> View
global s = producingKeyed s producer (\_ -> consuming (tutorial (TutorialRoute TutorialsR)))
  where
    producer s = do
      mt  <- App.req backend getTutorial s
      mtc <- App.req backend getTutorialContent s
      mts <-
        case mt of
          Just Tutorial {..} | Just s <- series, Nothing <- episode -> do
            ts <- App.req backend listTutorials ()
            let 
              inSeries Tutorial {..} = series == Just s
              isEpisode Tutorial {..} = isJust episode
              es = filter ((&&) <$> inSeries <*> isEpisode) ts
              es' = List.sortBy (compare `on` (\Tutorial { published } -> published)) es
            pure (Just es')
          _ -> 
            pure Nothing

      when (isNothing mt) do
        retitle "Not Found"

      pure do
        t  <- mt
        tc <- mtc
        ts <- maybe (Just []) Just mts
        pure (t,tc,ts)

package :: App.App => PackageName -> Types.Version -> Slug -> View
package pn v s = producingKeyed (pn,v,s) producer (\(pn,v,_) -> consuming (tutorial (PackageRoute (PackageTutorialR pn v TutorialsR))))
  where
    producer (pn,v,s) = do
      mt  <- App.req backend getPackageVersionTutorial (pn,v,s)
      mtc <- App.req backend getPackageVersionTutorialContent (pn,v,s)
      mts <-
        case mt of
          Just Tutorial {..} | Just s <- series, Nothing <- episode -> do
            ts <- App.req backend listTutorials ()
            let 
              inSeries Tutorial {..} = series == Just s
              isEpisode Tutorial {..} = isJust episode
              es = filter ((&&) <$> inSeries <*> isEpisode) ts
              es' = List.sortBy (compare `on` (\Tutorial { published } -> published)) es
            pure (Just es')
          _ -> 
            pure Nothing

      when (isNothing mt) do
        retitle "Not Found"

      pure do
        t  <- mt
        tc <- mtc
        ts <- maybe (Just []) Just mts
        pure (t,tc,ts)

tutorial :: App.App => Route -> Maybe (Tutorial Rendered,TutorialContent Rendered,[Tutorial Rendered]) -> View
tutorial rt = \case
  Nothing -> Problem.notFound "Tutorial"
  Just (Tutorial {..},TutorialContent md,ts) -> 
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
      , case ts of
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
                | Tutorial {..} <- ts
                , let Excerpt md = excerpt
                , let t = toSpecificTutorial rt slug
                ]
              ]
      ]

