module Pages.Version (page) where

import qualified App
import Components.Breadcrumbs (breadcrumbs)
import Components.Header (header)
import Components.PackageSearch (packageSearch)
import Control.Futures
import Data.Route as Route
import Styles.Themes

import Shared.Doc as Doc (Doc(meta),Meta(..),breakDoc)

import Pure.Elm.Application hiding (Doc,page,Frame)
import Pure.Spinners (ChasingDots(..))

import Data.Bool
import Data.Function ((&))
import Data.Maybe

page :: App.App => Txt -> Maybe Txt -> View
page pkg ver
  | Just v <- maybe (App.latest pkg) Just ver
  = Page $
      WithHeader (header (VersionR pkg ver) False) $
        Pages.Version.version pkg v (isNothing ver)

  | otherwise
  = Page $ WithHeader (header (VersionR pkg ver) False) $
      consumingWith options consumer Nothing
  where
    consumer :: Maybe () -> View
    consumer _ = error "impossible"

    options = defaultOptions
            & suspense (Milliseconds 500 0) loading
            & trouble  (Seconds 5 0) problems

data Version

version :: App.App => Txt -> Txt -> Bool -> View
version pkg ver latest =
  producingKeyed @Version (pkg,ver,latest) producer 
    (consumingWith options . consumer)
  where
    producer (p,v,_) = App.loadDoc (p,v)

    consumer = success

    options = defaultOptions 
            & suspense (Milliseconds 500 0) loading 
            & trouble  (Seconds 5 0) problems

loading :: View
loading = WithoutSidebar (Div <||> [ View (def @ChasingDots) ]) 

problems :: View
problems = WithoutSidebar (H2 <||> [ "Problem loading package." ])

success :: App.App => (Txt,Txt,Bool) -> Maybe Doc.Doc -> View
success (pkg,ver,latest) md
  | Just d <- md
  = doc d latest

  | otherwise
  = failed

failed :: View
failed = WithoutSidebar (H2 <||> [ "Package not found." ])

doc :: App.App => Doc -> Bool -> View
doc d@(Doc.meta -> Doc.Meta {..}) latest =
  let (desc,modules) = breakDoc d 
      v = bool (Just version) Nothing latest
   in WithHeader (breadcrumbs (VersionR package v)) $
        WithSidebar (packageSearch "Package" package v modules) $
          Div <| Theme MarkdownT |>
            [ processLinks d
            | d <- desc
            ]

