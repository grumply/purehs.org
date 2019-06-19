{-# LANGUAGE OverloadedStrings #-}
module Main where

import Purehsorg

import qualified Data.ByteString.Char8          as S8
import qualified Data.ByteString.Lazy           as L
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
-- import           Network.Wai.Middleware.Gzip
import           System.Environment
import           System.FilePath

import Shared (host,port)

import Control.Concurrent

main = do
  forkIO $ staticHTML5Server "./dist/site/exe:frontend/"
  purehsorg host port

staticHTML5Server root = run 80 app -- (compressing app)
  where
    -- compressing = gzip def { gzipFiles = GzipCacheFolder "/cache" }
    app req send =
      case pathInfo req of
        ["main.js"] -> fileServer req send
        ["favicon.ico"] -> fileServer req send
        _          -> let req' = req { pathInfo = ["index.html"] }
                      in fileServer req' send

    fileServer = staticApp (defaultFileServerSettings root)
