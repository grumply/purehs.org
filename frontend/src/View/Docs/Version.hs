module View.Docs.Version where

import Themes
import Types
import Utils
import View.Docs.Breadcrumbs (breadcrumbs)
import View.Docs.Searcher (searcher)

import Shared.Doc as Doc (Doc(meta,content),Meta(..),breakDoc)
import Shared.Cache as Cache (Cache(docs))
import Shared.Utils (breakMany)

import Pure.Data.Try
import Pure.Elm hiding (Doc)

import Data.List as List

version :: Model -> Txt -> Txt -> View
version model pkg ver
  | tdoc <- List.lookup (pkg,ver) (Cache.docs (cache model)) =
    case tdoc of
      Just (Done d) -> doc model d
      Just Trying   -> loading
      _             -> notExist
  | otherwise = notExist

notExist = Div <||> [ "Does not eixst." ]

loading = Div <||> [ "Loading." ]

doc :: Model -> Doc -> View
doc model d = let (desc,modules) = breakDoc d in
  Div <||>
    [ breadcrumbs (route model)
    , Div <| Theme DocT |>
      [ let Doc.Meta {..} = Doc.meta d
         in searcher package version modules
      , Div <| Theme MarkdownT |>
        fmap captureLocalRefs desc
      ]
    ]

