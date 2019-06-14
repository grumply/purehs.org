module View.Tutorials where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Theme

import Colors
import qualified Shared (Cache(..),Tutorial(..),TutorialMeta(..))
import Themes
import Types
import Utils

import Components.Header (header,headerOffset)
import Components.Icons  (logo)
import Components.Titler (titler)

import Control.Monad

tutorials :: Model -> View
tutorials model =
  Div <| Theme PageT . Theme TutorialsT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
      [ case route model of
          TutsR Nothing  -> listing model
          TutsR (Just t) -> tutorial t model
      ]
    , titler $
        case route model of
          TutsR (Just t) -> "Pure - " <> t
          _ -> "Pure - Tutorials"
    ]

listing model =
  Div <| Theme ListingT |>
    [ tutorialMeta tm
    | tm <- Shared.tutMetas (cache model)
    ]

tutorialMeta Shared.TutorialMeta {..} =
  let
    ref = "/tut/" <> slug
  in
    Div <| Theme MetaT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      ]

tutorial s model =
  Div <| Theme TutorialT |>
    [ case lookup s (Shared.tutorials (cache model)) of
        Just (Done t) -> success t
        Just Failed   -> failed
        _             -> loading
    ]

failed = 
  Div <| Theme FailedT |> 
    [ "Could not find tutorial" ]

success t =
  Div <| Theme MarkdownT . Theme SuccessT |> 
    (fmap captureLocalRefs (Shared.content t))

loading =
  Div <| Theme LoadingT |>
    [ "Loading tutorial" ]

data TutorialsT = TutorialsT
instance Themeable TutorialsT where
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
  theme c _ = void $ do
    is c $ do
      headerOffset
      apply $ do
        width       =: per 100
        maxWidth    =: pxs 1200
        marginLeft  =: auto
        marginRight =: auto
        padding     =: ems 1

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ is c .> do
    fontSize =: ems 3
    color =: darkGray

data MetaT = MetaT
instance Themeable MetaT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data TutorialT = TutorialT
instance Themeable TutorialT where
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
