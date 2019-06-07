module Pages.Docs where

import Pure.Data.CSS
import Pure.Data.Styles

import Colors
import Context
import Imports
import Shared (DocMeta(..))
import Themes

import Components.Header (viewHeader)
import Components.Titler (titler)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

data Env = Env

data State = State (Maybe [DocMeta])

newtype DocsM a = DocsM { runDocsM :: Aspect (Ctx DocsM) Env State a }
mkAspect ''DocsM

viewDocs :: Ctx DocsM -> View
viewDocs c = viewDocsM docs c Env (State Nothing)

docs :: DocsM View
docs = do
  State mdms <- get

  request <- prepare0 $ do
    GetDocMetasResponse dms <- proxyRequest GetDocMetasRequest
    put $ State $ Just dms

  when (isNothing mdms) $ do
    liftIO $ forkIO request
    put $ State $ Just []

  page

page :: DocsM View
page = do
  State mdms <- get

  c <- ctx >>= rebase

  cnt <- maybe loading success mdms

  pure $
    Div <| Theme PageT . Theme DocsT |>
      [ viewHeader (ffmap liftIO c)
      , Div <| Theme ContentT |>
        [ H1 <| Theme HeaderT |>
          [ "Docs" ]
        , cnt
        ]
      , titler [i|Pure - Docs|]
      ]

loading :: DocsM View
loading = do
  pure $
    Div <| Theme LoadingT |>
      [ [i|Loading Docs|] ]

success :: [DocMeta] -> DocsM View
success dms =
  pure $
    Div <| Theme SuccessT |>
      [ docMeta dm
      | dm <- dms
      ]
  where
    docMeta DocMeta {..} =
      let
        ref = [i|/doc/#{package}/#{version}|]
      in
        Div <| Theme MetaT . lref ref |>
          [ Div <| Theme PackageT |> [ text package ]
          , Div <| Theme VersionT |> [ text version ]
          ]

data DocsT = DocsT
instance Themeable DocsT where
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

data MetaT = MetaT
instance Themeable MetaT where
  theme c _ = void $ is c .> do
    cursor =: pointer

data PackageT = PackageT
instance Themeable PackageT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data VersionT = VersionT
instance Themeable VersionT where
  theme c _ = void $ is c $ return ()

