module Pages.Posts where

import Pure.Data.CSS
import Pure.Data.Styles

import Colors
import Context
import Imports
import Shared (PostMeta(..))
import Themes

import Components.Header (viewHeader)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env

data State = State (Maybe [PostMeta]) -- Just [] serves as a loading flag

newtype PostsM a = PostsM { runPostsM :: Aspect (Ctx PostsM) Env State a }
mkAspect ''PostsM

viewPosts :: Ctx PostsM -> View
viewPosts c = viewPostsM posts c Env (State Nothing)

posts :: PostsM View
posts = do
  State mpms <- get
  
  request <- prepare0 $ do
    GetPostMetasResponse pms <- proxyRequest GetPostMetasRequest
    put $ State $ Just pms

  when (isNothing mpms) $ do
    liftIO $ forkIO request
    put $ State (Just [])

  page

page :: PostsM View
page = do
  State mpms <- get

  c <- ctx >>= rebase

  cnt <- maybe loading success mpms

  pure $
    Div <| Theme PageT . Theme PostsT |>
      [ viewHeader (ffmap liftIO c)
      , Div <| Theme ContentT |>
        [ H1 <| Theme HeaderT |>
          [ "Posts" ]
        , cnt 
        ]
      , titler [i|Pure - Posts|]
      ]

loading :: PostsM View
loading = do
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading Posts|] ]

success :: [PostMeta] -> PostsM View
success pms =
  pure $
    Div <| Theme SuccessT |> 
      [ postMeta pm 
      | pm <- pms 
      ]
  where
    postMeta PostMeta {..} =
      let
        ref = [i|/blog/#{slug}|]
      in
        Div <| Theme PostT . lref ref |>
          [ Div <| Theme TitleT |> [ text title ]
          , Div <| Theme DateT  |> [ text year, "-", text month, "-" , text day ]
          ]

data PostsT = PostsT
instance Themeable PostsT where
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
      padding     =: ems 2
      marginLeft  =: auto
      marginRight =: auto
      marginTop   =: ems 2

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        fontSize =: pxs 60
        color =: darkGray
      atMedia "(max-width: 779px)" . is c .> do
        fontSize =: pxs 40

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data SuccessT = SuccessT
instance Themeable SuccessT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data PostT = PostT
instance Themeable PostT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data DateT = DateT
instance Themeable DateT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 1
      color =: darkGray
