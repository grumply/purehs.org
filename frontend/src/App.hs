{-# language ConstraintKinds #-}
module App where

import Control.Sync
import Data.Route as Route

import qualified Shared as API
import Shared.Cache as Cache

import Pure.Data.JSON as JSON
import qualified Pure.Data.Try as Try
import Pure.Data.Txt (Txt)
import qualified Pure.WebSocket as WS
import qualified Pure.Elm.Application as Elm

import Data.Map as Map

import Data.Maybe
import Data.Proxy
import Data.Functor.Identity

data Settings = Settings

data Message = Startup | Routed Route | RestoreScrollPosition | UpdateSession (Session -> Session) 

type App = (Elm.Session Session, Elm.Settings Settings, Elm.Elm Message Route) 

type Page = Elm.Page Settings Session Message Route.Route

data Session = Session
  { cache  :: Cache.Cache
  , socket :: WS.WebSocket
  }

emptySession :: Session
emptySession = Session mempty (error "Session socket not initialized")

traverseCache :: Traversal' Session Cache.Cache
traverseCache f (Session c r) = Session <$> f c <*> pure r

-- Little sneaky here with the wildcard constraint while keeping a signature. 
-- GHC can fill in the rest; this is just what's necessary for comprehension.
req :: ( WS.Request request, ToJSON payload, FromJSON response, _) 
    => Session -> Proxy request -> payload -> IO response
req ses rq pl = sync (WS.remote API.api (App.socket ses) rq pl)

load :: (WS.Request request, ToJSON payload, FromJSON response, WS.Rsp request ~ Maybe response, _) 
      => App 
      => Traversal' Cache.Cache (Map payload (Try.Try response))
      -> Proxy request 
      -> payload 
      -> IO ()
load f rq pl
  | isJust $ f check (cache Elm.session) = pure ()
  | otherwise = do
    let ins r = Elm.command $ UpdateSession (runIdentity . traverseCache (f (pure . Map.insert pl r)))
    ins Try.Trying
    fork $ req Elm.session rq pl >>= ins . tried
  where
    check m = do
      Map.lookup pl m
      pure m

loadTutorial :: App => Txt -> IO ()
loadTutorial = load Cache.traverseTutorials API.getTutorial

loadPost :: App => Txt -> IO ()
loadPost = load Cache.traversePosts API.getPost

loadDoc :: App => (Txt,Txt) -> IO ()
loadDoc = load Cache.traverseDocs API.getDoc

loadPackage :: App => Txt -> IO ()
loadPackage = load Cache.traversePackages API.getPackage

loadPage :: App => Txt -> IO ()
loadPage = load Cache.traversePages API.getPage

tried :: Maybe a -> Try.Try a
tried = maybe Try.Failed Try.Done
