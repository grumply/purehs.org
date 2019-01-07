module Pages.Examples (examplesPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Router
import Pure.Theme

import Containers.Examples
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

examplesPage :: PageScope => View
examplesPage =
  Div <| Theme ExamplesPageT . Theme PageT |>
    [ header
    , Div <| Theme ExamplesContainerT |>
      [ H1 <| Theme ExamplesHeaderT |>
        [ "Examples" ]
      , container loading examples
      ]
    ]

loading =
  Div <| Theme LoadingT

examples es =
  Div <| Theme ExamplesT |>
    (es <&> example)

example Example {..} =
  Div <| Theme MarkdownT . Theme ExampleT |>
    [ H2  <| Theme TitleT |> [ text (emTitle eMeta) ]
    , Div <| Theme ContentT |> eContent
    , Div <| Theme CodeT |> eCode
    ]

data ExamplesPageT = ExamplesPageT
instance Themeable ExamplesPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data ExamplesContainerT = ExamplesContainerT
instance Themeable ExamplesContainerT where
  theme c _ = void $ do
    is c $ do
      headerOffset

      apply $ do
        maxWidth   =: pxs 1200
        margin     =: auto
        padding    =: ems 1

data ExamplesHeaderT = ExamplesHeaderT
instance Themeable ExamplesHeaderT where
  theme c _ = void $ is c .> do
    fontSize =: ems 3
    color =: darkGray

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data ExamplesT = ExamplesT
instance Themeable ExamplesT where
  theme c _ = void $ is c $ return ()

data ExampleT = ExampleT
instance Themeable ExampleT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ is c .> do
    marginTop =: ems 2
    color =: darkLavender

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $ is c $ return ()

data CodeT = CodeT
instance Themeable CodeT where
  theme c _ = void $ is c $ return ()
