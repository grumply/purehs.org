{-# language ConstraintKinds #-}
module App where

import Data.Route as Route

import qualified Shared as API

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Time
import qualified Pure.WebSocket as WS
import qualified Pure.Elm.Application as Elm

import Data.Map as Map

import Control.Concurrent
import Control.Concurrent.Async (Async,async)
import Data.IORef
import Data.Proxy
import Data.Typeable
import System.IO.Unsafe
import Unsafe.Coerce

data Settings = Settings

data Message = Routed Route | UpdateSession (Session -> Session) 

type App = (Elm.Session Session, Elm.Settings Settings, Elm.Elm Message Route) 

type Page = Elm.Page Settings Session Message Route.Route

data Session = Session
  { socket :: WS.WebSocket
  }

mkSession :: WS.WebSocket -> Session
mkSession = Session

-- traverseCache :: Lens.Traversal' Session Cache.Cache
-- traverseCache f (Session c r) = Session <$> f c <*> pure r

data RequestMap
  = forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
  => RequestMap (Proxy rq) (Map pl (WS.Rsp rq))

{-# NOINLINE cache #-}
cache :: IORef (Map TypeRep RequestMap)
cache = unsafePerformIO (newIORef mempty)

addToCache :: forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
           => Proxy rq -> pl -> WS.Rsp rq -> IO ()
addToCache p rq rsp = modifyIORef' cache (Map.alter alt (typeRep (Proxy :: Proxy rq)))
  where
    alt Nothing = Just (RequestMap p $ Map.singleton rq rsp)
    alt (Just (RequestMap _ rm)) = Just (RequestMap p $ Map.insert rq rsp $ unsafeCoerce rm)

lookupInCache :: forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
              => Proxy rq -> pl -> IO (Maybe (WS.Rsp rq))
lookupInCache _ rq = do
  c <- readIORef cache
  pure $
    case Map.lookup (typeOf (undefined :: rq)) c of
      Just (RequestMap _ rm) -> Map.lookup rq $ unsafeCoerce rm
      _                      -> Nothing

-- Little sneaky here with the wildcard constraint while keeping a signature. 
-- GHC can fill in the rest; this is just what's necessary for comprehension.
req :: ( Ord payload, WS.Request request, ToJSON payload, FromJSON response, _) 
    => Session -> Proxy request -> payload -> IO (Either response (Async response))
req ses rq pl = do
  mrsp <- lookupInCache rq pl
  case mrsp of
    Nothing -> Right <$> do
      async $ do
        mv <- newEmptyMVar
        WS.remote API.api (App.socket ses) rq pl $ \rsp -> do
          forkIO (addToCache rq pl rsp)
          putMVar mv rsp
        takeMVar mv
    Just r ->
      pure (Left r)

{-
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
-}
