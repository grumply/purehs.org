module Pages.Post (page) where

import qualified App 
import Components.Header (header)
import Control.Futures
import Data.Route
import Shared.Post as Post (Post(content))
import Styles.Themes

import Pure.Elm.Application as Elm hiding (page,Frame)
import Pure.Spinners (ChasingDots(..))

import Data.Function ((&))

page :: App.App => Txt -> View
page s = Page $ WithHeader (header BlogR False) (post s)

post :: App.App => Txt -> View
post s =
  producingKeyed @Post s producer 
    (consumingWith options . consumer)
  where
    producer = App.loadPost

    consumer _ = success

    options = defaultOptions 
            & suspense (Milliseconds 300 0) loading 
            & trouble  (Seconds 3 0) problems

loading :: View
loading = WithoutSidebar (View (def @ChasingDots))

problems :: View
problems = WithoutSidebar "Problem loading post."

failed :: View
failed = WithoutSidebar "Post not found."

success :: App.App => Maybe Post.Post -> View
success Nothing = failed
success (Just p) =
  WithoutSidebar $
    Div <| Theme MarkdownT |>
      [ processLinks c
      | c <- Post.content p
      ]
