module Main where

import Shared
import Connection

import Pure.Elm (body,inject,delay,pattern Minutes)
import Pure.Server (pattern Server)

import Control.Monad
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  inject body (Server host port connection)
  sleep
  where
    sleep = forever (delay (Minutes 3 0))
