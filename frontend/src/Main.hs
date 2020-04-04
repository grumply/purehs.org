{-# language CPP #-}
module Main where

import qualified API
import qualified App
import qualified Pages.About as About
import qualified Pages.Blog as Blog
import qualified Pages.Blog.Post as Post
import qualified Pages.Docs as Docs
import qualified Pages.Home as Home
import qualified Pages.Tutorials as Tutorials
import qualified Pages.Tutorials.Tutorial as Tutorial

#ifdef __GHCJS__
import qualified Shared
#endif

import Styles.Themes

import Data.Route as Route

import Pure.Elm.Application
import qualified Pure.WebSocket as WS

import Data.Functor

main :: IO ()
main = inject body (Div <| Theme AppT |> [ run app App.Settings ]) 
  where
    app = App [App.Startup] [] [App.Routed] [] App.emptySession update view

update :: Elm App.Message Route => Route.Route -> App.Message -> App.Settings -> App.Session -> IO App.Session
update _rt App.Startup _ ses = do
  ws <- WS.websocket
  WS.enact ws API.impl 
#ifdef __GHCJS__
  WS.activate ws Shared.host Shared.port False
#endif
  pure ses { App.socket = ws }

update _rt (App.UpdateSession f) _ ses = 
  pure (f ses)

update _old (App.Routed _new) _ ses = 
  command App.RestoreScrollPosition $> ses

update _rt App.RestoreScrollPosition _ ses =
  addAnimation restoreScrollPosition $> ses

view :: Route.Route -> App.Page
view NoR               = \_ _ -> Null
view AboutR            = page About.about
view HomeR             = page Home.home
view TutorialsR        = page Tutorials.tutorials
view BlogR             = page Blog.blog
view (PostR p)         = page (Post.post p)
view DocsR             = page (Docs.docs DocsR)
view (PackageR p)      = page (Docs.docs (PackageR p))
view (VersionR p v)    = page (Docs.docs (VersionR p v))
view (ModuleR p v m)   = page (Docs.docs (ModuleR p v m))
view (EntityR p v m e) = page (Docs.docs (EntityR p v m e))
view (TutorialR t)     = page (Tutorial.tutorial t)


