module Pages.Docs.Module (packageModule) where

import qualified App
import Control.With
import Components.Breadcrumbs (breadcrumbs)
import Data.Route as Route
import Pages.Docs.Styles
import Pages.Docs.PackageSearch (packageSearch)
import Styles.Themes

import Shared.Cache as Cache (Cache(docs))
import Shared.Doc as Doc (Doc(meta,content),Meta(..),breakDoc)
import Shared.Utils (breakMany)

import Pure.Data.Txt as Txt
import Pure.Data.Try
import Pure.Elm.Application hiding (Doc)

import Data.Bool
import Data.List as List

packageModule :: App.App => Txt -> Txt -> Txt -> Bool -> View
packageModule pkg ver mdl latest
  | tdoc <- List.lookup (pkg,ver) (Cache.docs (App.cache session)) =
    case tdoc of
      Just (Done d) -> doc mdl d latest
      Just Trying   -> loading
      _             -> notExist
  | otherwise = notExist

notExist = Div <||> [ "Does not eixst." ]

loading = Div <||> [ "Loading." ]

doc :: App.App => Txt -> Doc -> Bool -> View
doc mdl d latest = let (_,mds) = breakDoc d in 
  case findModule mdl mds of
    [] -> notExist
    (md:_) 
      | Doc.Meta {..} <- Doc.meta d 
      , v <- bool (Just version) Nothing latest
      -> Div <||>
          [ breadcrumbs (ModuleR package v mdl)
          , Div <| Theme DocT |>
            [ packageSearch package v [md]
            , Div <| Theme MarkdownT |>
              fmap processLinks (linkEntities package v mdl $ safeTail md)
            ]
          ]
  where
    safeTail [] = []
    safeTail (_ : xs) = xs

findModule :: Txt -> [[View]] -> [[View]]
findModule mdl = List.filter match
  where
    match m@(Children [TextView _ t] H2 : _) = t == mdl
    match _ = False

linkEntities pkg ver mdl = go
  where
    go [] = []
    go (Children [t@(TextView _ e)] H3 : rest) = H3 <||> [ A <| url Href Href (location (EntityR pkg ver mdl e)) |> [ t ] ] : go rest
    go (x : xs) = x : go xs

