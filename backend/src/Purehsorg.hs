module Purehsorg where

import Connection

import Pure.Elm (body,inject)
import Pure.WebSocket as WS
import Server (pattern Server)

import Control.Concurrent
import Control.Monad
import System.IO

purehsorg :: String -> Int -> IO ()
purehsorg host port = do
  inject body (Server host port connection)
  hSetBuffering stdout LineBuffering
  sleep
  where
    sleep = forever (threadDelay (6 * 10 ^ 10))

