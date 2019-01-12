module Pages.Tutorials (tutorialsPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.Txt.Interpolate
import Pure.Router
import Pure.Theme

import Containers.Tutorials
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

tutorialsPage :: PageScope => View
tutorialsPage =
  Div <| Theme TutorialsPageT . Theme PageT |>
    [ header
    , Div <| Theme TutorialsContainerT |>
      [ H1 <| Theme TutorialsHeaderT |>
        [ "Tutorials - WIP" ]
      , container loading tutorialMetas
      ]
    , fetcher
    ]

loading =
  Div <| Theme LoadingT

tutorialMetas pms =
  Div <| Theme TutorialsT |> (fmap tutorialMeta pms)

tutorialMeta TutorialMeta {..} =
  let
    ref = [i|/tut/#{slug}|]
  in
    Div <| Theme TutorialT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      ]

data TutorialsPageT = TutorialsPageT
instance Themeable TutorialsPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data TutorialsContainerT = TutorialsContainerT
instance Themeable TutorialsContainerT where
  theme c _ = void $
    is c .> do
      width       =: per 100
      maxWidth    =: pxs 1200
      marginLeft  =: auto
      marginRight =: auto
      padding     =: ems 1

data TutorialsHeaderT = TutorialsHeaderT
instance Themeable TutorialsHeaderT where
  theme c _ = void $ is c .> do
    fontSize =: ems 3
    color =: darkGray

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data TutorialsT = TutorialsT
instance Themeable TutorialsT where
  theme c _ = void $ is c $ return ()

data TutorialT = TutorialT
instance Themeable TutorialT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2
