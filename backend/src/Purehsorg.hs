module Purehsorg where

import Pure hiding (ask,get)
import Pure.Capability
import Pure.Capability.TH
import Pure.Data.Try
import Pure.Server
import Pure.WebSocket as WS
import qualified Shared

import Control.Monad
import Control.Concurrent
import Data.IORef
import Network.Socket as Network
import System.IO

import Control.Lens
import Control.Lens.Operators
import Control.Lens.TH

import Services.Docs as Docs
import Services.Pages as Pages
import Services.Posts as Posts
import Services.Tutorials as Tutorials
import Context

data ConnEnv = ConnEnv WebSocket

data ConnState = ConnState 
  { _enacted :: Bool
  }
makeLenses ''ConnState

newtype ConnM a = ConnM { runConnM :: Aspect (Ctx ConnM) ConnEnv ConnState a }
mkAspect ''ConnM

data AppEnv = AppEnv String Int

data AppState = AppState 
  { _loaded :: Bool
  }
makeLenses ''AppState

newtype AppM a = AppM { runAppM :: Aspect (Ctx AppM) AppEnv AppState a }
mkAspect ''AppM

viewApp :: Ctx AppM -> String -> Int -> View
viewApp ctx host port = viewAppM app ctx (AppEnv host port) (AppState False)

app :: AppM View
app = do
  AppEnv host port <- ask
  AppState loaded <- get
  unless loaded $ do
    loadDocs
    loadPages
    loadPosts
    loadTutorials
  c <- ctx >>= rebase
  pure $
    Server host port $ \ws ->
      viewConn (ffmap liftIO c) ws

viewConn :: Ctx ConnM -> WebSocket -> View
viewConn ctx ws = viewConnMStatic conn ctx (ConnEnv ws) (ConnState False)

ip :: ConnM Txt
ip = do
  ConnEnv ws <- ask
  Just (getIP -> ip,_,_,_) <- liftIO $ wsSocket <$> readIORef ws
  pure ip
  where
    getIP :: Network.SockAddr -> Txt
    getIP (SockAddrInet _ ha) = toTxt (show ha)
    getIP (SockAddrInet6 _ _ ha _) = toTxt (show ha)
    getIP _ = error "getIP: Invalid socket type; cannot extract address."

conn :: ConnM View
conn = do
  env@(ConnEnv ws) <- ask
  ConnState active <- get
  ref <- sref
  c <- ctx
  cache <- buildCache
  unless active $ do
    liftIO $ do
      notify Shared.clientApi ws Shared.setCache cache
      enact ws (impl c env ref)
      activate ws
    enacted #= True
  -- do something here?
  pure Null

buildCache = 
  Shared.Cache 
    <$> getPostMetas 
    <*> getDocMetas 
    <*> getTutorialMetas 
    <*> pure [] 
    <*> pure [] 
    <*> pure [] 
    <*> getPages
  where
    getPages = do
      mp <- lookupPage "about"
      pure $
        case mp of
          Nothing -> []
          Just p  -> [("about",Done p)]

type Conn = SRef ConnState

runConn :: Ctx ConnM -> ConnEnv -> Conn -> ConnM a -> IO a
runConn ctx env conn connm = evalAspect (runConnM connm) ctx env conn

purehsorg :: String -> Int -> IO ()
purehsorg host port = do
  inject body (viewApp productionCtx host port)
  hSetBuffering stdout LineBuffering
  sleep
  where
    sleep = forever (threadDelay (6 * 10 ^ 10))

-- API Implementation

impl ctx env conn = Impl Shared.api msgs reqs
  where
    msgs = WS.none
    reqs = handleReloadMarkdown ctx env conn <:>
           handleGetPost ctx env conn <:>
           handleGetTutorial ctx env conn <:>
           handleGetDoc ctx env conn <:>
           handleGetPage ctx env conn <:>
           WS.none

handleGetPost :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetPost
handleGetPost ctx env conn = respondWith $ runConn ctx env conn . Posts.lookupPost

handleGetTutorial :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetTutorial
handleGetTutorial ctx env conn = respondWith $ runConn ctx env conn . Tutorials.lookupTutorial

handleGetDoc :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetDoc
handleGetDoc ctx env conn = respondWith $ runConn ctx env conn . Docs.lookupDoc . uncurry Shared.DocMeta

handleGetPage :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetPage
handleGetPage ctx env conn = respondWith $ runConn ctx env conn . Pages.lookupPage

handleReloadMarkdown :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.ReloadMarkdown
handleReloadMarkdown ctx env conn = respondWith $ const $ runConn ctx env conn reload
  where
    reload = do
      loadDocs
      loadPosts
      loadTutorials
