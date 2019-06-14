module Client where

import Pure.WebSocket as WS
import Shared

import System.IO.Unsafe

{-# NOINLINE client #-}
client = unsafePerformIO (WS.clientWS host port)
