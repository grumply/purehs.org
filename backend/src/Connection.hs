module Connection where

import Data.Cached
import Services.Caches

import Shared

import Pure.Data.JSON (encodeBS)
import Pure.Elm
import Pure.WebSocket as WS

import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

data Model = Model

data Msg = Startup

connection :: WebSocket -> View
connection = run app
  where
    app = App [Startup] [] [] mdl update view
    mdl = Model
    view _ _ = Null

update :: Elm Msg => Msg -> WebSocket -> Model -> IO Model
update msg ws mdl =
  case msg of
    Startup -> do
      enact ws impl
      activate ws
      c <- cached rawCache
      sendRaw ws (buildEncodedDispatchByteString (messageHeader setCache) c)
      pure mdl

impl = Impl Shared.api msgs reqs
  where
    msgs = WS.none
    reqs = handleGetPost <:>
           handleGetTutorial <:>
           handleGetDoc <:>
           handleGetPage <:>
           WS.none

handleGetPost :: RequestHandler Shared.GetPost
handleGetPost = respondWithRaw $ \k -> do
  rps <- cached rawPost
  pure $ fromMaybe "null" (Map.lookup k rps)

handleGetTutorial :: RequestHandler Shared.GetTutorial
handleGetTutorial = respondWithRaw $ \k -> do
  rts <- cached rawTutorial
  pure $ fromMaybe "null" (Map.lookup k rts)

handleGetDoc :: RequestHandler Shared.GetDoc
handleGetDoc = respondWithRaw $ \k -> do
  rds <- cached rawDoc
  pure $ fromMaybe "null" (Map.lookup k rds)

handleGetPage :: RequestHandler Shared.GetPage
handleGetPage = respondWithRaw $ \k -> do
  rps <- cached rawPage
  pure $ fromMaybe "null" (Map.lookup k rps)