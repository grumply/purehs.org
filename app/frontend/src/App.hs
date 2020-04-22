{-# language ConstraintKinds #-}
module App where

import Control.Sync
import Data.Route as Route

import qualified Shared as API
import Shared.Cache as Cache
import qualified Shared.Page as Page
import qualified Shared.Post as Post
import qualified Shared.Doc as Doc
import qualified Shared.Tutorial as Tutorial
import qualified Shared.Package as Package

import Pure.Data.JSON as JSON
import qualified Pure.Data.Try as Try
import Pure.Data.Txt (Txt)
import qualified Pure.WebSocket as WS
import qualified Pure.Elm.Application as Elm

import Data.Map as Map

import Control.Lens as Lens
import Data.List as List
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

traverseCache :: Lens.Traversal' Session Cache.Cache
traverseCache f (Session c r) = Session <$> f c <*> pure r

-- Little sneaky here with the wildcard constraint while keeping a signature. 
-- GHC can fill in the rest; this is just what's necessary for comprehension.
req :: ( WS.Request request, ToJSON payload, FromJSON response, _) 
    => Session -> Proxy request -> payload -> IO response
req ses rq pl = sync (WS.remote API.api (App.socket ses) rq pl)

load :: (WS.Request request, ToJSON payload, FromJSON response, WS.Rsp request ~ Maybe response, _) 
     => App 
     => Lens.Traversal' Cache.Cache (Map payload (Try.Try response))
     -> Proxy request 
     -> payload 
     -> IO (Maybe response)
load f rq pl
  | a <- Lens.view f (cache Elm.session)
  , Just x <- Map.lookup pl a 
  = pure (Try.try Nothing Nothing Just x)

  | otherwise = do
    let ins r = Elm.command $ UpdateSession (runIdentity . traverseCache (f (pure . Map.insert pl r)))
    rsp <- req Elm.session rq pl 
    ins (tried rsp)
    pure rsp

loadTutorial :: App => Txt -> IO (Maybe Tutorial.Tutorial)
loadTutorial = load Cache.traverseTutorials API.getTutorial

loadPost :: App => Txt -> IO (Maybe Post.Post)
loadPost = load Cache.traversePosts API.getPost

loadDoc :: App => (Txt,Txt) -> IO (Maybe Doc.Doc)
loadDoc = load Cache.traverseDocs API.getDoc

loadPackage :: App => Txt -> IO (Maybe Package.Package)
loadPackage = load Cache.traversePackages API.getPackage

loadPage :: App => Txt -> IO (Maybe Page.Page)
loadPage = load Cache.traversePages API.getPage

tried :: Maybe a -> Try.Try a
tried = maybe Try.Failed Try.Done

loadPackageAndDoc :: App => Txt -> Txt -> IO ()
loadPackageAndDoc p v = do
  App.loadPackage p
  App.loadDoc (p,v)
  pure ()

latest :: App => Txt -> Maybe Txt
latest p
  | docMetas <- Cache.docMetas (cache Elm.session)
  , versions <- List.filter ((== p) . Doc.package) docMetas
  , latest   <- fmap Doc.version (listToMaybe (List.reverse versions))
  = latest