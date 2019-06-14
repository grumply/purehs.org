module View.Docs where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Theme

import Colors
import qualified Shared (Cache(..),Doc(..),DocMeta(..))
import Themes
import Types
import Utils

import Components.Header (header,headerOffset)
import Components.Icons  (logo)
import Components.Titler (titler)

import Control.Monad

docs :: Model -> View
docs model =
  Div <| Theme PageT . Theme DocsT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
        [ case route model of
            DocsR Nothing      -> listing model
            DocsR (Just (p,v)) -> doc p v model
        ]
    , titler $
        case route model of
          DocsR (Just (p,v)) -> "Pure - " <> p <> "-" <> v
          _ -> "Pure - Docs"
    ]

listing model = 
  Div <| Theme ListingT |>
    [ docMeta dm
    | dm <- Shared.docMetas (cache model)
    ]

docMeta Shared.DocMeta {..} =
  let
    ref = "/doc/" <> package <> "/" <> version
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme PackageT |> [ text package ]
      , Div <| Theme VersionT |> [ text version ]
      ]

doc p v model =
  Div <| Theme DocT |>
    [ case lookup (p,v) (Shared.docs (cache model)) of
        Just (Done d) -> success d
        Just Failed   -> failed
        _             -> loading
    ]

failed =
  Div <| Theme FailedT |>
    [ "Could not find doc" ]

success d =
  Div <| Theme MarkdownT . Theme SuccessT |>
    (fmap captureLocalRefs (Shared.content d))

loading =
  Div <| Theme LoadingT |>
    [ "Loading doc" ]

data DocsT = DocsT
instance Themeable DocsT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data ListingT = ListingT
instance Themeable ListingT where
  theme c _ = void $
    is c $ do
      headerOffset
      apply $ do
        width       =: per 100
        maxWidth    =: pxs 1200
        padding     =: ems 2
        marginLeft  =: auto
        marginRight =: auto
        marginTop   =: ems 2

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $
    is c $ do
      headerOffset
      apply $ do
        width       =: per 100
        maxWidth    =: pxs 1200
        padding     =: ems 2
        marginLeft  =: auto
        marginRight =: auto
        marginTop   =: ems 2

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ is c .> do
    fontSize =: ems 3
    color =: darkGray

data MetaT = MetaT
instance Themeable MetaT where
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

data DocT = DocT
instance Themeable DocT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()

data SuccessT = SuccessT
instance Themeable SuccessT where
  theme c _ = void $ is c $ return ()
