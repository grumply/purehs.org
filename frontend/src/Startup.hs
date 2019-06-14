{-# LANGUAGE NoMonomorphismRestriction #-}
module Startup where

import Pure.Elm
import Pure.WebSocket as WS

import Client
import Shared
import Types

import Control.Monad

foreign import javascript unsafe
  "console.log($1)" log_txt :: Txt -> IO ()

startup = void $ enact client impl 

impl = Impl Shared.clientApi msgs reqs
  where
    msgs = handleSetCache <:> WS.none
    reqs = WS.none

handleSetCache :: Elm Msg => MessageHandler SetCache
handleSetCache = awaiting $ do
  liftIO $ log_txt "Got Cache"
  c <- acquire
  liftIO $ command (SetCache c)
