module Pages.Docs.Version where

import qualified App
import Components.Breadcrumbs (breadcrumbs)
import Data.Route as Route
import Pages.Docs.Styles
import Pages.Docs.PackageSearch (packageSearch)
import Styles.Themes

import Shared.Doc as Doc (Doc(meta),Meta(..),breakDoc)
import Shared.Cache as Cache (Cache(docs))

import Pure.Data.Try
import Pure.Elm.Application hiding (Doc)

import Data.Bool
import Data.List as List

version :: App.App => Txt -> Txt -> Bool -> View
version pkg ver latest
  | tdoc <- List.lookup (pkg,ver) (Cache.docs (App.cache session)) =
    case tdoc of
      Just (Done d) -> doc d latest
      Just Trying   -> loading
      _             -> notExist
  | otherwise = notExist

notExist :: View
notExist = Div <||> [ "Does not eixst." ]

loading :: View
loading = Div <||> [ "Loading." ]

doc :: App.App => Doc -> Bool -> View
doc d@(Doc.meta -> Doc.Meta {..}) latest =
  let (desc,modules) = breakDoc d 
      v = bool (Just version) Nothing latest
   in Div <||>
      [ breadcrumbs (VersionR package v)
      , Div <| Theme DocT |>
        [ packageSearch package v modules
        , Div <| Theme MarkdownT |>
          fmap processLinks desc
        ]
      ]

