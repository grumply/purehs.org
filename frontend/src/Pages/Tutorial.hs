module Pages.Tutorial (tutorialPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.Txt as Txt
import Pure.Router
import Pure.Theme

import Containers.Tutorial
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

tutorialPage :: (TutScope, PageScope) => View
tutorialPage =
  Div <| Theme TutorialPageT . Theme PageT |>
    [ header
    , Div <| Theme TutorialContainerT |>
      [ container loading tutorial
      ]
    ]

loading =
  Div <| Theme LoadingT

tutorial Nothing =
  Div <| Theme NoTutorialT |>
    [ "Tutorial not found." ]

tutorial (Just Tutorial { meta = TutorialMeta {..}, ..}) =
  Div <| Theme MarkdownT . Theme TutorialT |>
    content

data TutorialPageT = TutorialPageT
instance Themeable TutorialPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data TutorialContainerT = TutorialContainerT
instance Themeable TutorialContainerT where
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

data NoTutorialT = NoTutorialT
instance Themeable NoTutorialT where
  theme c _ = void $ is c $ return ()

data TutorialT = TutorialT
instance Themeable TutorialT where
  theme c _ = void $ is c $ return ()
