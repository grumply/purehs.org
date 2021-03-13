module Page where

import App (req,App)
import Components.Markdown (markdown)
import Components.Problem (notFound)
import Styles.Themes (Load)

import Shared (getPage, getPageContent, backend)
import Shared.Page (Page, PageContent(..))
import Shared.Types (Slug)

import Pure.Elm.Application hiding (Page)
import Pure.Maybe (consuming, producingKeyed)

import Control.Monad (when)
import Data.Maybe (isNothing)

page :: App.App => Slug -> View
page pg = producingKeyed pg producer (\_ -> consuming consumer)
  where 
    producer pg = do
      mp  <- App.req backend getPage pg
      mpc <- App.req backend getPageContent pg
      when (isNothing mp) do
        retitle "Page Not Found"
      pure mpc

    consumer (Just (PageContent md)) = 
      markdown md <| Themed @Page . Themed @Load

    consumer Nothing = 
      notFound "Page"

instance Theme Page where
  theme c =
    is c do
      padding-top =: 60px
      padding-bottom =: 60px

