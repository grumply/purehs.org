{-# language DuplicateRecordFields, UndecidableInstances, DeriveAnyClass, ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI , GHCForeignImportPrim #-}
module Main where

import Shared

import App
import Cache
import Data.Route
import qualified Styles.Themes as Themes

import qualified Author
import qualified Authors
import qualified Blog
import qualified Entity
import qualified Home
import qualified Module
import qualified Package
import qualified Packages
import qualified Page
import qualified Post
import qualified Tutorial
import qualified Tutorials
import qualified Version

import qualified Components.Breadcrumbs as Breadcrumbs
import qualified Components.Header as Header

import Styles.Themes

import Pure.Elm.Application as Pure hiding (Settings,Session)
import qualified Pure.WebSocket as WS

import Control.Concurrent
import System.IO
import Prelude hiding (max,reverse)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ws <- WS.clientWS Shared.host Shared.port
  inject body (cache ws)
  inject body (Div <| Themed @Themes.App |> [ run (app ws) App.Settings ])
  where
    app ws = App [App.Startup] [] [App.Routed] [] (pure (App.mkSession ws)) update Main.view

update :: Route -> Message -> Settings -> Session -> IO Session
update _ App.Startup _ ses =
  pure ses

update _rt (App.UpdateSession f) _ ses = 
  pure (f ses)

update _old (App.Routed _new) _ ses = do
  for_ (Pure.title _new) retitle 
  forkIO do
    void do
      -- delay the restoration by a few frames
      delay (Millisecond * 100) 
      addAnimation restoreScrollPosition
  pure ses

view :: Route -> App.Page
view r = Pure.page $ 
  case r of
    NoRoute -> Null

    -- Home page is a bit too custom to collapse this case.
    HomeRoute -> Home.home

    _ -> 
      Themes.page $ 
        withHeader (Header.header r) $ 
          Div <||>
            [ case r of
                PageRoute _ -> Null
                _ -> 
                  Header <| Themed @PageHeader |> 
                    [ Nav <||> Breadcrumbs.breadcrumbs r 
                    , Nav <||> Breadcrumbs.sublinks r
                    ]
            , app r
            ]

app :: App.App => Route -> View
app = \case

    PageRoute s -> Page.page s

    BlogRoute r -> case r of
      BlogR   -> Blog.global
      PostR s -> Post.global s

    TutorialRoute r -> case r of
      TutorialsR -> Tutorials.global
      TutorialR s -> Tutorial.global s

    AuthorRoute r -> case r of 
      AuthorsR -> Authors.authors
      AuthorR a -> Author.author a
      AuthorBlogR a -> Blog.author a
      AuthorTutorialsR a -> Tutorials.author a
      AuthorPackagesR a -> Packages.author a

    PackageRoute r -> case r of
      PackagesR -> Packages.global
      PackageR p -> Package.package p
      PackageBlogR p r -> 
        case r of
          BlogR   -> Blog.package p
          PostR s -> Post.package p s
      PackageVersionR p v -> Version.version p v
      PackageTutorialR p v r ->
        case r of
          TutorialsR  -> Tutorials.version p v
          TutorialR s -> Tutorial.package p v s
      PackageModuleR p v m -> Module.module_ p v m
      PackageEntityR p v m e -> Entity.entity p v m e

    _ -> Null