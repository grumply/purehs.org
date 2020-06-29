module Pages.Page where

import Components.Article
import Components.Markdown
import Components.Title as Title

import Data.Placeholders
import Data.Render
import Data.Resource
import Data.Route
import Styles.Themes hiding (wait)

import Shared.Page as Page
import Shared.Types

import Pure.Elm.Application hiding (render,wait,Page)
import Pure.Maybe

import Control.Concurrent.Async (wait)

import Control.Monad
import GHC.Exts (IsList(..))

instance Render (Route, (Request (Maybe Page), Request (Maybe (PageContent Rendered)))) where
  render (rt,(pv,pcv)) =
    producing (either titled (wait >=> titled) pv) 
      (consumingWith options (consumer True))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ Nothing = notFound "Page"
      consumer b (Just p) = render (rt,pcv) 

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Just placeholderPageView) <| Themed @PlaceholderT)

instance Render (Route,Request (Maybe (PageContent Rendered))) where
  render (_,pcv) = 
    producing (either pure wait pcv) 
      (consumingWith options consumer)
    where
      consumer Nothing = Null
      consumer (Just (PageContent md)) = render md <| Themed @PageArticleT

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer (Just placeholderPageContentView) <| Themed @PlaceholderT)

data PageArticleT
instance Theme PageArticleT where
  theme c = void $
    is c .> do
      padding-top =: 60px
      padding-bottom =: 60px