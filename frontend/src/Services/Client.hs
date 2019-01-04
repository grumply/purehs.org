{-# LANGUAGE NoMonomorphismRestriction #-}
module Services.Client where

import Pure.WebSocket

import App
import Shared

import System.IO.Unsafe

{-# NOINLINE client #-}
client = unsafePerformIO (clientWS host port)

req_ api = remote api client

req = req_ Shared.api

