{-# language ConstraintKinds #-}
module App where

import Cache ( Msg(Request) )

import Data.Route as Route ( Route )

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.WebSocket as WS ( API, WebSocket, Request(Rsp) )
import Pure.Maybe ( producingKeyed )
import qualified Pure.Elm.Application as Elm

import Control.Concurrent ( newEmptyMVar, putMVar, takeMVar )
import Data.Proxy ( Proxy )

data Settings = Settings

data Message = Startup | Routed Route.Route | UpdateSession (Session -> Session) 

type App = (Elm.Session Session, Elm.Settings Settings) 

type Page = Elm.Page Settings Session Message Route.Route

data Session = Session
  { socket :: WS.WebSocket
  }

mkSession :: WS.WebSocket -> Session
mkSession = Session

req :: (Ord payload, WS.Request request, ToJSON payload, FromJSON response, _) 
    => WS.API msgs reqs -> Proxy request -> payload -> IO response
req api rq pl = do
  mv <- newEmptyMVar
  Elm.publish (Request api rq pl pure (putMVar mv))
  takeMVar mv

with :: forall request msgs reqs payload response. (Ord payload, WS.Rsp request ~ response, _ ) 
     => WS.API msgs reqs
     -> Proxy request 
     -> payload 
     -> (payload -> Maybe response -> Elm.View) 
     -> Elm.View
with api rq pl f = producingKeyed pl (req api rq) f
