module Pages.Post where

import Pure.Data.CSS
import Pure.Data.Styles

import Colors
import Context
import Imports
import Shared (Post(..),PostMeta(..))
import Themes
import Utils

import Components.Header (viewHeader,headerOffset)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env Txt

data State = State (Maybe (Try Post))

newtype PostM a = PostM { runPostM :: Aspect (Ctx PostM) Env State a }
mkAspect ''PostM

viewPost :: Ctx PostM -> Txt -> View
viewPost c slug = viewPostM post c (Env slug) (State Nothing)

post :: PostM View
post = do
  Env slug <- ask
  State mtp <- get

  request <- prepare0 $ do
    GetPostResponse mp <- proxyRequest $
      GetPostRequest slug
    put $ State $ Just $ maybe Failed Done mp

  when (isNothing mtp) $ do
    liftIO $ forkIO request
    put $ State (Just Trying)

  page

page :: PostM View
page = do
  Env slug <- ask
  State mtp <- get

  c <- ctx >>= rebase

  cnt <- maybe loading (try loading failed success) mtp

  pure $
    Div <| Theme PageT . Theme PostT |>
      [ viewHeader (ffmap liftIO c)
      , Div <| Theme ContentT |>
        [ cnt ]
      , titler [i|Pure - #{slug}|]
      ]

loading :: PostM View
loading = do
  Env slug <- ask
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading #{slug}|] ]

failed :: PostM View
failed = do
  Env slug <- ask
  pure $
    Div <| Theme FailedT |>
      [ [i|Could not find post #{slug}|] ]

success :: Post -> PostM View
success Post { meta = PostMeta {..}, .. } =
  pure $
    Div <| Theme MarkdownT . Theme SuccessT |>
      (fmap captureLocalRefs content)

--------------------------------------------------------------------------------
 
data PostT = PostT
instance Themeable PostT where
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
  theme c _ = void $ do
    is c $ do
      has "h2" .> do
        marginTop =: ems 2
        padding =: pxs 8
        backgroundColor =: lightLavender

