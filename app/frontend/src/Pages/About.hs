module Pages.About (page) where

import qualified App
import Components.Header
import Control.Futures
import Data.Function ((&))
import Data.Route as Route
import Shared.Page as Page (Page(content))
import Styles.Themes

import Pure.Elm.Application hiding (content,page,Frame)
import Pure.Spinners (ChasingDots(..))

page :: App.App => View
page = 
  Page $
    WithHeader (header AboutR False) $
      WithoutSidebar $ 
        Div <||> 
          [ about 
          , users 
          ]

data About

about :: App.App => View
about =
  producing @About producer 
    (consumingWith options consumer)
  where
    producer = App.loadPage "about"

    consumer = success

    options = defaultOptions 
            & suspense (Milliseconds 300 0) loading 
            & trouble  (Seconds 2 0) problems

success :: App.App => Maybe Page.Page -> View
success Nothing = failed
success (Just pg) =
  Div <| Theme MarkdownT |>
    fmap processLinks (Page.content pg)

loading :: View
loading = WithoutSidebar (View (def @ChasingDots))

problems :: View
problems = WithoutSidebar "Problem loading page."

failed :: View
failed = WithoutSidebar "Page not found."

users :: View
users = Div <||> []
