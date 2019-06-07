module Pages.Tutorial where

import Pure.Data.CSS
import Pure.Data.Styles

import Colors
import Context
import Imports
import Shared (Tutorial(..),TutorialMeta(..))
import Themes
import Utils

import Components.Header (viewHeader,headerOffset)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env Txt

data State = State (Maybe (Try Tutorial))

newtype TutorialM a = TutorialM { runTutorialM :: Aspect (Ctx TutorialM) Env State a }
mkAspect ''TutorialM

viewTutorial :: Ctx TutorialM -> Txt -> View
viewTutorial ctx slug = viewTutorialM tutorial ctx (Env slug) (State Nothing)

tutorial :: TutorialM View
tutorial = do
  Env slug <- ask
  State mtt <- get

  request <- prepare0 $ do
    GetTutorialResponse mt <- proxyRequest $
      GetTutorialRequest slug
    put $ State $ Just $ maybe Failed Done mt

  when (isNothing mtt) $ do
    liftIO $ forkIO request 
    put $ State (Just Trying)

  page

page :: TutorialM View
page = do
  Env slug <- ask
  State mtt <- get

  c <- ctx >>= rebase

  cnt <- maybe loading (try loading failed success) mtt

  pure $
    Div <| Theme PageT . Theme TutorialT |>
      [ viewHeader (ffmap liftIO c)
      , Div <| Theme ContentT |>
        [ cnt ]
      , titler [i|Pure - #{slug}|]
      ]

loading :: TutorialM View
loading = do
  Env slug <- ask
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading #{slug}|] ]

failed :: TutorialM View
failed = do
  Env slug <- ask
  pure $
    Div <| Theme FailedT |>
      [ [i|Could not find tutorial #{slug}|] ]

success :: Tutorial -> TutorialM View
success Tutorial { meta = TutorialMeta {..}, .. } =
  pure $
    Div <| Theme MarkdownT . Theme SuccessT |>
      (fmap captureLocalRefs content)

--------------------------------------------------------------------------------

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

data ContentT = ContentT
instance Themeable ContentT where
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

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()

data SuccessT = SuccessT
instance Themeable SuccessT where
  theme c _ = void $ is c $ return ()
