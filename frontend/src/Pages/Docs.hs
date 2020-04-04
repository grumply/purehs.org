module Pages.Docs (docs) where

import qualified App
import Components.Header (header)
import Control.With
import Data.Route as Route
import Pages.Docs.Entity (entity)
import Pages.Docs.Packages (packages)
import Pages.Docs.Package (package)
import Pages.Docs.Version (version)
import Pages.Docs.Module (packageModule)
import Styles.Themes

import qualified Shared.Cache as Cache (Cache(docMetas))
import qualified Shared.Doc as Doc (Meta(..))

import Pure.Elm.Application

import Data.List as List
import Data.Maybe
import Data.Foldable

docs :: App.App => Route.Route -> View
docs rt =
  Div <| Theme PageT |>
    [ header DocsR False
    , Pages.Docs.page rt
    ]

page :: App.App => Route.Route -> View
page rt = 
  Div <| Theme ContentT |>
    [ case rt of
        PackageR p                            -> with rt (App.loadPackage p) (package p)
        VersionR p (Just v)                   -> with rt (load p v) (version p v False)
        ModuleR  p (Just v) m                 -> with rt (load p v) (packageModule p v m False)
        EntityR  p (Just v) m e               -> with rt (load p v) (entity p v m e False)
        VersionR p _     | Just v <- latest p -> with rt (load p v) (version p v True)
        ModuleR  p _ m   | Just v <- latest p -> with rt (load p v) (packageModule p v m True)
        EntityR  p _ m e | Just v <- latest p -> with rt (load p v) (entity p v m e True)
        _                                     -> packages 
    ]
  where
    load p v = App.loadPackage p >> App.loadDoc (p,v)
    latest p = let c = App.cache session in
      fmap Doc.version $ listToMaybe $ List.reverse $ 
        List.filter ((== p) . Doc.package) (Cache.docMetas c)
