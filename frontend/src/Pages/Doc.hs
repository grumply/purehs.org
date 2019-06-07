module Pages.Doc where

import Pure.Data.CSS
import Pure.Data.Styles

import Colors
import Context
import Imports hiding (Doc)
import Shared (Doc(..),DocMeta(..))
import Themes
import Utils

import Components.Header (viewHeader,headerOffset)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env Txt Txt

data State = State (Maybe (Try Doc))

newtype DocM a = DocM { runDocM :: Aspect (Ctx DocM) Env State a }
mkAspect ''DocM

viewDoc :: Ctx DocM -> Txt -> Txt -> View
viewDoc c pkg ver = viewDocM doc c (Env pkg ver) (State Nothing)

doc :: DocM View
doc = do
  Env pkg ver <- ask
  State mtd <- get

  request <- prepare0 $ do
    GetDocResponse md <- proxyRequest $
      GetDocRequest pkg ver
    put $ State $ Just $ maybe Failed Done md

  when (isNothing mtd) $ do
    liftIO $ forkIO request 
    put $ State (Just Trying)

  page

page :: DocM View
page = do
  Env pkg ver <- ask
  State mtd <- get

  c <- ctx >>= rebase

  cnt <- maybe loading (try loading failed success) mtd

  pure $
    Div <| Theme PageT . Theme DocT |>
        [ viewHeader (ffmap liftIO c)
        , Div <| Theme ContentT |> 
          [ cnt ]
        , titler [i|Pure - #{pkg}-#{ver}|]
        ]

loading :: DocM View
loading = do
  Env pkg ver <- ask
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading #{pkg}-#{ver}|] ]

failed :: DocM View
failed = do
  Env pkg ver <- ask
  pure $
    Div <| Theme FailedT |>
      [ [i|Could not find documentation for #{pkg}-#{ver}|] ]

success :: Doc -> DocM View
success Doc { meta = DocMeta {..}, .. } = 
  pure $
    Div <| Theme MarkdownT . Theme SuccessT |>
      (fmap captureLocalRefs content)

--------------------------------------------------------------------------------

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
