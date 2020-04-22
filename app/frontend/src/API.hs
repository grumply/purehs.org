module API where

import qualified App
import qualified Shared
import Data.Route

import Pure.WebSocket as WS
import Pure.Elm.Application

impl :: Elm App.Message Route => _
impl = WS.Impl Shared.clientApi msgs reqs
  where
    msgs = handleSetCache WS.<:> WS.none
    reqs = WS.none

handleSetCache :: Elm App.Message Route => WS.MessageHandler Shared.SetCache
handleSetCache = WS.awaiting $ do
  cache <- WS.acquire
  liftIO $ command (App.UpdateSession (\c -> c { App.cache = cache }))
