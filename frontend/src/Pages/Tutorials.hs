module Pages.Tutorials where

import Pure.Data.Styles
import Pure.Data.CSS

import Colors
import Context
import Imports
import Shared (TutorialMeta(..))
import Themes

import Components.Header (viewHeader)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env

data State = State (Maybe [TutorialMeta])

newtype TutorialsM a = TutorialsM { runTutorialsM :: Aspect (Ctx TutorialsM) Env State a }
mkAspect ''TutorialsM

viewTutorials :: Ctx TutorialsM -> View
viewTutorials c = viewTutorialsM tutorials c Env (State Nothing)

tutorials :: TutorialsM View
tutorials = do
  State mtms <- get

  request <- prepare0 $ do
    GetTutorialMetasResponse tms <- proxyRequest 
      GetTutorialMetasRequest
    put $ State $ Just tms

  when (isNothing mtms) $ do
    liftIO $ forkIO request
    put $ State $ Just []

  page

page :: TutorialsM View
page = do
  State mtms <- get

  c <- ctx >>= rebase

  cnt <- maybe loading success mtms

  pure $
    Div <| Theme PageT . Theme TutorialsT |>
      [ viewHeader (ffmap liftIO c)
      , Div <| Theme ContentT |>
        [ H1 <| Theme HeaderT |>
          [ "Tutorials" ]
        , cnt
        ]
      , titler [i|Pure - Tutorials|]
      ]

loading :: TutorialsM View
loading = do
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading Tutorials|] ]

success :: [TutorialMeta] -> TutorialsM View
success tms = 
  pure $
    Div <| Theme SuccessT |>
      [ tutorialMeta tm
      | tm <- tms
      ]
  where
    tutorialMeta TutorialMeta {..} =
      let
        ref = [i|/tut/#{slug}|]
      in
        Div <| Theme TutorialT . lref ref |>
          [ Div <| Theme TitleT |> [ text title ]
          ]

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

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $
    is c .> do
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

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data SuccessT = SuccessT
instance Themeable SuccessT where
  theme c _ = void $ is c $ return ()

data TutorialT = TutorialT
instance Themeable TutorialT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2
