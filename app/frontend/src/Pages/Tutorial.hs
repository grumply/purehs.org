module Pages.Tutorial (page) where

import qualified App 
import Components.Header (header)
import Control.Futures
import Data.Route as Route
import Styles.Themes

import Shared.Tutorial as Tutorial (Tutorial(content))

import Pure.Elm.Application as Elm hiding (page,Frame)
import Pure.Spinners (ChasingDots(..))

import Data.Function ((&))

page :: App.App => Txt -> View
page s =
  Page $
    WithHeader (header (TutorialR s) False) $
      tutorial s

tutorial :: App.App => Txt -> View
tutorial s = 
  producingKeyed @Tutorial s producer 
    (consumingWith options . consumer)
  where
    producer = App.loadTutorial

    consumer _ = success

    options = defaultOptions 
            & suspense (Milliseconds 300 0) loading 
            & trouble  (Seconds 3 0) problems

loading :: View
loading = WithoutSidebar (View (def @ChasingDots))

problems :: View
problems = WithoutSidebar "Problem loading tutorial."

failed :: View
failed = WithoutSidebar "Tutorial not found."

success :: App.App => Maybe Tutorial.Tutorial -> View
success Nothing = failed
success (Just pg) =
  WithoutSidebar $
    Div <| Theme MarkdownT |>
      [ processLinks c
      | c <- Tutorial.content pg
      ]

