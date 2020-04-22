module Main where

import qualified API
import App
import qualified Pages.About as About
import qualified Pages.Blog as Blog
import qualified Pages.Post as Post
import qualified Pages.Docs as Docs
import qualified Pages.Entity as Entity
import qualified Pages.Module as Module
import qualified Pages.Package as Package
import qualified Pages.Version as Version
import qualified Pages.Home as Home
import qualified Pages.Tutorials as Tutorials
import qualified Pages.Tutorial as Tutorial

import qualified Shared

import Styles.Themes

import Data.Route

import Pure.Elm.Application hiding (Session,Settings)
import qualified Pure.WebSocket as WS

import Control.Applicative
import Data.Functor
import Data.Maybe

import Debug.Trace

main :: IO ()
main = inject body (Div <| Theme AppT |> [ run app App.Settings ]) 
  where
    app = App [App.Startup] [] [App.Routed] [] App.emptySession update view

update :: Elm Message Route => Route -> Message -> Settings -> Session -> IO Session
update _rt App.Startup _ ses = do
  ws <- WS.websocket
  WS.enact ws API.impl 
  WS.activate ws Shared.host Shared.port False
  pure ses { App.socket = ws }

update _rt (App.UpdateSession f) _ ses = 
  pure (f ses)

update _old (App.Routed _new) _ ses = 
  -- delay the restoration by one command batch
  command App.RestoreScrollPosition $> ses

update _rt App.RestoreScrollPosition _ ses =
  addAnimation restoreScrollPosition $> ses

view :: Route -> App.Page
view r = page $
  case r of
    NoR             -> Null
    AboutR          -> About.page
    HomeR           -> Home.page
    TutorialsR      -> Tutorials.page
    BlogR           -> Blog.page
    PostR p         -> Post.page p
    PackageR p      -> Package.page p
    TutorialR t     -> Tutorial.page t
    VersionR p v    -> Version.page p v
    ModuleR p v m   -> Module.page p v m
    EntityR p v m e -> Entity.page p v m e
    DocsR           -> Docs.page
