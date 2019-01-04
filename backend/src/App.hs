{-# LANGUAGE ImplicitParams, RankNTypes, ViewPatterns, NoMonomorphismRestriction, StandaloneDeriving, BangPatterns, TypeApplications, LambdaCase, TypeApplications, ScopedTypeVariables, GADTs, FlexibleContexts #-}
module App (module App, module Export) where

import Shared

import Pure hiding (update,modify,Left,Right,ref)
import Pure.Data.JSON as Export (ToJSON,FromJSON)
import Pure.Data.JSON (toJSON,logJSON)
import Pure.Server
import Pure.WebSocket as WS

import Control.Concurrent
import Control.Monad.Reader as R
import Control.Monad.State
import Control.Monad.State as Export hiding (State,withState,state)
import Control.Monad.Trans as Export
import Data.Foldable
import Data.IORef
import Data.Typeable

import Control.Lens as Export hiding ((<|),(|>),(<||>))
import GHC.Generics as Export (Generic)

import Network.Socket as Network
import System.IO

type AppRef st = Ref IO () st
type ConnRef as cs = Ref IO (WebSocket,as) cs
type AppM st = StateT st IO
type AppView st = (?app :: AppRef st) => View
type AppComponent st a = (?app :: AppRef st) => a -> View

-- | This works because ?app cascades through the application (though
-- not through typeclass dictionaries)
update :: (?app :: AppRef st, ToJSON st) => AppM st a -> IO (MVar a)
update f = do
    mv <- newEmptyMVar
    modifyM_ ?app $ \props st -> do
        (a,!st') <- runStateT f st
        putMVar mv a
        -- logJSON st'
        return (st',return ())
    return mv

update_ :: (?app :: AppRef st, ToJSON st) => AppM st a -> IO ()
update_ f = do
    modifyM_ ?app $ \props st -> do
        !st' <- execStateT f st
        -- logJSON st'
        return (st',return ())

getIP :: Network.SockAddr -> Txt
getIP (SockAddrInet _ ha) = toTxt (show ha)
getIP (SockAddrInet6 _ _ ha _) = toTxt (show ha)
getIP _ = error "getIP: Invalid socket type; cannot extract address."

deriving instance Show CloseReason
deriving instance Show Status

run :: Typeable appState
    => String
    -> Int
    -> appState
    -> ((?app :: AppRef appState) => ConnRef appState connState -> Implementation msgs rqs msgs' rqs')
    -> ((?app :: AppRef appState) => Txt -> IO connState)
    -> ((?app :: AppRef appState) => appState -> connState -> View)
    -> IO ()
run host port initial impl f c = do
    mv <- newEmptyMVar
    inject body $ flip ComponentIO () $ \self -> let ?app = self in
        def
            { construct = return initial
            , render = \_ st ->
                View $ Server host port $ \ws -> flip ComponentIO (ws,st) $ \conn -> def
                    { construct = do
                        (ws,st) <- Pure.ask conn
                        Just (getIP -> ip,_,_,_) <- wsSocket <$> readIORef ws
                        f ip
                    , executing = void $ do
                        (ws,st) <- Pure.ask conn
                        enact ws (impl conn)
                        WS.activate ws
                    , render = \(ws,as) cs -> c as cs
                    }
            , unmounted = putMVar mv ()
            }
    takeMVar mv

type Responding app conn request response a = ReaderT request (ReaderT (ConnRef app conn,IO (),Either LazyByteString response -> IO ()) IO) a

req :: Responding app conn request response request
req = R.ask

done :: Responding app conn request response ()
done = do
    (_,d,_) <- lift R.ask
    lift (lift d)

send :: response -> Responding app conn request response ()
send rsp = do
    (_,_,s) <- lift R.ask
    lift (lift (s $ Right rsp))

ref :: Responding app conn request response (ConnRef app conn)
ref = lift (R.asks (Export.view _1))

app :: Responding app conn request response app
app = ref >>= liftIO . Pure.ask <&> Export.view _2

conn :: Responding app conn request response conn
conn = ref >>= liftIO . Pure.get

modifyConn :: (app -> conn -> conn) -> Responding app conn request response ()
modifyConn f = do
    r <- ref
    liftIO (modify_ r $ \(_,app) conn -> f app conn)

modifyApp :: ToJSON app => (?app :: AppRef app) => AppM app r -> Responding app conn request response (MVar r)
modifyApp f = liftIO $ update f

respond :: forall rqTy request response app conn.
              ( Request rqTy
              , Req rqTy ~ (Int,request)
              , Identify (Req rqTy)
              , I (Req rqTy) ~ Int
              , FromJSON request
              , Rsp rqTy ~ response
              , ToJSON response
              )
           => Responding app conn request response () -> ConnRef app conn -> RequestHandler rqTy
respond rspndng ref = responds (Proxy @ rqTy) $ \done -> \case
    Left dsp           -> logJSON dsp
    Right (rsp,(_,rq)) -> runReaderT (runReaderT rspndng rq) (ref,done,void . rsp)
