module Purehsorg where

import Pure hiding (ask,get)
import Pure.Capability
import Pure.Capability.TH
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
import Services.Examples as Examples
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
    loadExamples
    loadPosts
    loadTutorials
  c <- ctx >>= rebase
  pure $
    Server host port $ \ws ->
      viewConn (ffmap liftIO c) ws

viewConn :: Ctx ConnM -> WebSocket -> View
viewConn ctx ws = viewConnM conn ctx (ConnEnv ws) (ConnState False)

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
  unless active $ do
    liftIO $ do
      enact ws (impl c env ref)
      activate ws
    enacted #= True
  -- do something here?
  pure Null

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
           handleGetPostMetas ctx env conn <:>
           handleGetTutorialMetas ctx env conn <:>
           handleGetDocMetas ctx env conn <:>
           handleGetExamples ctx env conn <:>
           WS.none

handleGetPost :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetPost
handleGetPost ctx env conn = respondWith $ runConn ctx env conn . Posts.lookupPost

handleGetTutorial :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetTutorial
handleGetTutorial ctx env conn = respondWith $ runConn ctx env conn . Tutorials.lookupTutorial

handleGetDoc :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetDoc
handleGetDoc ctx env conn = respondWith $ runConn ctx env conn . Docs.lookupDoc . uncurry Shared.DocMeta

handleGetPostMetas :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetPostMetas
handleGetPostMetas ctx env conn = respondWith $ const $ runConn ctx env conn Posts.getPostMetas

handleGetTutorialMetas :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetTutorialMetas
handleGetTutorialMetas ctx env conn = respondWith $ const $ runConn ctx env conn Tutorials.getTutorialMetas

handleGetDocMetas :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetDocMetas
handleGetDocMetas ctx env conn = respondWith $ const $ runConn ctx env conn Docs.getDocMetas

handleGetExamples :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.GetExamples
handleGetExamples ctx env conn = respondWith $ const $ runConn ctx env conn Examples.getExamples

handleReloadMarkdown :: Ctx ConnM -> ConnEnv -> Conn -> RequestHandler Shared.ReloadMarkdown
handleReloadMarkdown ctx env conn = respondWith $ const $ runConn ctx env conn reload
  where
    reload = do
      loadDocs
      loadExamples
      loadPosts
      loadTutorials
