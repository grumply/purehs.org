module Purehsorg where

import Connection

import Pure.Elm (body,inject,delay,pattern Minutes)
import Pure.Server (pattern Server)

import Control.Monad
import System.IO

purehsorg :: String -> Int -> IO ()
purehsorg host port = do
  inject body (Server host port connection)
  hSetBuffering stdout LineBuffering
  sleep
  where
    sleep = forever (delay (Minutes 3 0))

