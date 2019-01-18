{-# LANGUAGE QuasiQuotes #-}
module Pages.Docs (docsPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.Txt as Txt
import Pure.Data.Txt.Interpolate
import Pure.Router
import Pure.Theme

import Containers.Docs
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

docsPage :: PageScope => View
docsPage =
  Div <| Theme DocsPageT . Theme PageT |>
    [ header
    , Div <| Theme DocsContainerT |>
      [ H1 <| Theme DocsHeaderT |>
        [ "Documentation - WIP" ]
      , fetcher docMetas
      ]
    ]

docMetas dms =
  Div <| Theme DocMetasT |> (fmap docMeta dms)

docMeta DocMeta {..} =
  let
    ref = [i|/doc/#{package}/#{version}|]
  in
    Div <| Theme DocMetaT . lref ref |>
      [ Div <| Theme PackageT |> [ text package ]
      , Div <| Theme VersionT |> [ text version ]
      ]

data DocsPageT = DocsPageT
instance Themeable DocsPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data DocsContainerT = DocsContainerT
instance Themeable DocsContainerT where
  theme c _ = void $
    is c .> do
      width       =: per 100
      maxWidth    =: pxs 1200
      marginLeft  =: auto
      marginRight =: auto
      padding     =: ems 1

data DocsHeaderT = DocsHeaderT
instance Themeable DocsHeaderT where
  theme c _ = void $ is c .> do
    fontSize =: ems 3
    color =: darkGray

data DocMetasT = DocMetasT
instance Themeable DocMetasT where
  theme c _ = void $ is c $ return ()

data DocMetaT = DocMetaT
instance Themeable DocMetaT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data PackageT = PackageT
instance Themeable PackageT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data VersionT = VersionT
instance Themeable VersionT where
  theme c _ = void $ is c $ return ()

