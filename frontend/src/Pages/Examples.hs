module Pages.Examples where

import Pure.Data.CSS
import Pure.Data.Styles

import Colors
import Context
import Imports
import Shared (Example(..),ExampleMeta(..))
import Themes

import Components.Header (viewHeader,headerOffset)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env

data State = State (Maybe [Example])

newtype ExamplesM a = ExamplesM { runExamplesM :: Aspect (Ctx ExamplesM) Env State a }
mkAspect ''ExamplesM

viewExamples :: Ctx ExamplesM -> View
viewExamples c = viewExamplesM examples c Env (State Nothing)

examples :: ExamplesM View
examples = do
  State mes <- get

  request <- prepare0 $ do
    GetExamplesResponse es <- proxyRequest GetExamplesRequest
    put $ State $ Just es

  when (isNothing mes) $ do
    liftIO $ forkIO request
    put $ State (Just [])

  page

page :: ExamplesM View
page = do
  State mes <- get

  c <- ctx >>= rebase

  cnt <- maybe loading success mes

  pure $
    Div <| Theme PageT . Theme ExamplesT |>
      [ viewHeader (ffmap liftIO c)
      , Div <| Theme ContentT |>
        [ H1 <| Theme HeaderT |>
          [ "Examples" ]
        , cnt
        ]
      , titler [i|Pure - Examples|]
      ]
      
loading :: ExamplesM View
loading = do
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading Examples|] ]

success :: [Example] -> ExamplesM View
success es =
  pure $
    Div <| Theme SuccessT |>
      [ example e
      | e <- es
      ]
  where
    example Example { meta = ExampleMeta {..}, .. } =
      Div <| Theme MarkdownT . Theme ExampleT |>
        [ Div <| Theme ExampleContentT |> content
        ]

data ExamplesT = ExamplesT
instance Themeable ExamplesT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $ do
    is c $ do
      headerOffset

      apply $ do
        maxWidth   =: pxs 1200
        margin     =: auto
        padding    =: ems 1

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ is c .> do
    marginTop =: ems 0.2
    fontSize =: ems 3
    color =: darkGray

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data SuccessT = SuccessT
instance Themeable SuccessT where
  theme c _ = void $ is c $ return ()

data ExampleT = ExampleT
instance Themeable ExampleT where
  theme c _ = void $ is c $ return ()

data ExampleContentT = ExampleContentT
instance Themeable ExampleContentT where
  theme c _ = void $ do
    is c . has "h2" .> do
      color =: darkLavender

