module Pages.Doc (docPage) where

import Pure hiding (Doc,content)
import Pure.Data.CSS
import Pure.Data.Txt as Txt
import Pure.Router
import Pure.Theme

import Containers.Doc
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

import Debug.Trace

docPage :: (DocScope, PageScope) => View
docPage = withDoc $ \d -> traceShow d $
  Div <| Theme DocPageT . Theme PageT |>
    [ header
    , Div <| Theme DocContainerT |>
      [ container loading doc
      ]
    , fetcher
    ]

loading =
  Div <| Theme LoadingT

doc Nothing =
  Div <| Theme NoDocT |>
    [ "Doc not found." ]

doc (Just Doc { meta = DocMeta {..}, ..}) =
  Div <| Theme MarkdownT . Theme DocT |>
    (fmap captureLocalRefs content)

data DocPageT = DocPageT
instance Themeable DocPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data DocContainerT = DocContainerT
instance Themeable DocContainerT where
  theme c _ = void $ do
    is c $ do
      headerOffset

      apply $ do
        maxWidth   =: pxs 1200
        margin     =: auto
        padding    =: ems 1

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data NoDocT = NoDocT
instance Themeable NoDocT where
  theme c _ = void $ is c $ return ()

data DocT = DocT
instance Themeable DocT where
  theme c _ = void $ is c $ return ()
