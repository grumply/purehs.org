-- |
-- Module      :  Cache
-- Copyright   :  Sean Hickman 2020
-- License     :  BSD3
--
-- Maintainer  :  sean@grumply.com
-- Stability   :  experimental
-- Portability :  JavaScript
module Cache (cache,Msg(..)) where

import Pure.Elm.Application ( pattern Applet, pattern Null, command, subscribe, View, Elm, run )
import Pure.Data.JSON ( ToJSON, FromJSON )
import Pure.WebSocket as WS ( request, API, type (∈), WebSocket, Request(Rsp, Req) )

import Data.Map as Map ( Map, empty, insert, lookup, singleton )

import Data.Foldable ( for_ )
import Data.Proxy ( Proxy )
import Data.Typeable ( typeRep, TypeRep )
import Unsafe.Coerce ( unsafeCoerce )

data RequestMap
  = forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
  => RequestMap (Proxy rq) (Map.Map pl (Either [WS.Rsp rq -> IO ()] (WS.Rsp rq)))

data Model = Model
  { responses :: Map.Map TypeRep RequestMap -- ^ A map from the TypeRep of the Proxy of the payload to a RequestMap
  }

data Msg
  = forall rq rqs msgs pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, ToJSON pl, FromJSON (Rsp rq), rq ∈ rqs ~ True)
  => Request (WS.API msgs rqs) (Proxy rq) pl (WS.Rsp rq -> IO (WS.Rsp rq)) (WS.Rsp rq -> IO ())

  | forall rq pl rsp. (WS.Request rq, WS.Req rq ~ (Int,pl), WS.Rsp rq ~ rsp, Ord pl)
  => Satisfy (Proxy rq) pl rsp

  | Startup

cache :: WS.WebSocket -> View
cache = run (Applet [Startup] [] [] (pure mdl) upon view)
  where
    mdl = Model Map.empty
    
    upon :: Elm Msg => Msg -> WS.WebSocket -> Model -> IO Model
    upon Startup _ mdl = do
      subscribe
      pure mdl
      
    upon (Request api p pl process cb) ws mdl = do
      case Map.lookup (typeRep p) (responses mdl) of
        Nothing -> do
          WS.request api ws p pl $ \rsp -> do
            rsp' <- process rsp
            command (Satisfy p pl rsp')
          pure mdl 
            { responses = Map.insert (typeRep p) (RequestMap p (Map.singleton pl ((Left [cb])))) (responses mdl)
            }
            
        Just (RequestMap _ rm) ->
          -- that's a lot of coercions
          case Map.lookup pl (unsafeCoerce rm) of
            Just (Left cbs) -> do
              let 
                cbs' = cbs ++ [unsafeCoerce cb]
                rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs') (unsafeCoerce rm)))
              pure mdl
                { responses = Map.insert (typeRep p) rm' (responses mdl)
                }

            Just (Right rsp) -> do
              cb rsp
              pure mdl

            Nothing -> do
              WS.request api ws p pl (command . Satisfy p pl)
              let 
                cbs = [cb]
                rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs) (unsafeCoerce rm)))
              pure mdl
                { responses = Map.insert (typeRep p) rm' (responses mdl)
                }

    upon (Satisfy p pl rsp) _ mdl = do
      case Map.lookup (typeRep p) (responses mdl) of
        Just (RequestMap _ rm) ->
          case Map.lookup (unsafeCoerce pl) rm of
            Just (Left cbs) -> do
              for_ @[] (unsafeCoerce cbs) ($ rsp)
              pure mdl
                { responses = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (responses mdl)
                } 

            Just (Right _) -> do
              pure mdl
              
            Nothing -> do
              -- This case allows for pre-seeding the cache with responses.
              pure mdl
                { responses = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (responses mdl)
                } 

        Nothing -> do
          pure mdl
          
    view _ _ = Null