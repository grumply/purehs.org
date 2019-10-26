module View.Docs.Module (packageModule) where

import Themes
import Types
import Utils

import Shared.Cache as Cache (Cache(docs))
import Shared.Doc as Doc (Doc(meta,content),Meta(..),breakDoc)
import Shared.Utils (breakMany)

import Pure.Data.Txt as Txt
import Pure.Data.Try
import Pure.Elm hiding (Doc)

import View.Docs.Breadcrumbs (breadcrumbs)
import View.Docs.Searcher (searcher)

import Data.List as List

packageModule :: Model -> Txt -> Txt -> Txt -> View
packageModule model pkg ver mdl
  | tdoc <- List.lookup (pkg,ver) (Cache.docs (cache model)) =
    case tdoc of
      Just (Done d) -> doc model mdl d
      Just Trying   -> loading
      _             -> notExist
  | otherwise = notExist

notExist = Div <||> [ "Does not eixst." ]

loading = Div <||> [ "Loading." ]

doc :: Model -> Txt -> Doc -> View
doc model mdl d = let (_,mds) = breakDoc d in
  Div <||>
    [ breadcrumbs (route model)
    , case findModule mdl mds of
        [] -> notExist
        (md:_) | Doc.Meta {..} <- Doc.meta d ->
          Div <| Theme DocT |>
            [ searcher package version mds
            , Div <| Theme MarkdownT |>
              fmap captureLocalRefs (safeTail md)
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

