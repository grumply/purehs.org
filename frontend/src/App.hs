{-# LANGUAGE ImplicitParams, TypeOperators, ConstraintKinds #-}
module App (module App, module Export) where

import Shared

import Pure hiding (update,modify)
import Pure.Data.JSON as Export (logJSON,ToJSON,FromJSON)
import Pure.Router as Export
import Pure.Router.Internal
import Pure.Theme
import Pure.WebSocket as WS hiding ((<||>))

import Control.Concurrent
import Control.Monad.State
import Control.Monad.State as Export hiding (State,state,withState)
import Control.Monad.Trans as Export
import Data.Foldable
import Data.Kind
import Data.Typeable
import Data.Unique
import Control.Lens as Export hiding (Context,(<|),(|>),(.>))
import Control.Lens.At as Export
import GHC.Generics as Export (Generic)

import Pure.Cache

import Data.Maybe
import Debug.Trace

-- A component reference with `rt` for props and `st` for state.
type AppRef rt st = Ref IO rt st

-- Core scoping types.
-- ScopedApp is used on initialization of router and startup code.
-- ScopedPage is used after initialization of app state and router setup and
--   throughout the life of the application.
type ScopedApp rt st = (?app :: AppRef rt st)
type ScopedPage rt st = (ScopedApp rt st, ?route :: rt, ?state :: st, ?scope :: [Txt])

-- An update type used when updating application state.
type AppM st = StateT st IO

data Context m p s super = Context
    { cProps :: p
    , cState :: s
    , cSelf  :: Ref m p s
    , cSuper :: super
    }

type ContextIO p s super = Context IO p s super

context :: (?context :: Context m p s super) => Context m p s super
context = ?context

-- Requires TypeApplications at use site.
withContext :: forall super m p s. (?context :: super)
            => Ref m p s -> (p -> s -> ((?context :: Context m p s super) => View)) -> (p -> s -> View)
withContext self f = \p s -> let super = ?context in let ?context = Context p s self super in f p s

newContext :: Ref m p s -> (p -> s -> ((?context :: Context m p s ()) => View)) -> (p -> s -> View)
newContext self f = \p s -> let ?context = Context p s self () in f p s

-- Update the application state from IO as if we have access to a
-- state monad transformer containing the app state.
update :: (ScopedApp rt st, ToJSON st) => AppM st a -> IO ()
update f = modifyM_ ?app $ \props st -> do
    st' <- execStateT f st
    logJSON st'
    return (st',return ())

type ContextApp rt st = (?context :: ContextIO rt st (ContextIO () rt ()))

run :: (Typeable rt)
    => st
    -> rt
    -> (ScopedApp rt st => Routing rt)
    -> (ScopedApp rt st => IO ())
    -> ((ContextApp rt st, ScopedPage rt st) => a -> (Caching => View))
    -> ((ContextApp rt st, ScopedPage rt st) => rt -> (Caching => a))
    -> IO ()
run initial route routes startup page pages =
  inject body $ flip ComponentIO () $ \self ->
    def
      { construct = return route
      , executing = void $ onRoute' $ \rt -> do
          modify_ self $ \_ _ -> rt
      , render = newContext self $ \_ rt ->
          flip ComponentIO rt $ \self -> let ?app = self in
            def
              { construct = return initial
              , executing = startup
              , render = withContext self $ \rt st -> caching $
                let ?state = st
                    ?route = rt
                    ?scope = []
                in
                  Div <| Height (per 100) |>
                    [ View (Router route (Export.route routes))
                    , page (pages rt)
                    ]
              }
      }

-- this debugging approach won't work with asynchronous request handling
remote :: ( Request rqTy
          , Req rqTy ~ (Int,request)
          , ToJSON request
          , ToJSON response
          , Rsp rqTy ~ response
          , FromJSON response
          , (rqTy WS.∈ rqs) ~ 'True
          )
       => FullAPI msgs rqs
       -> WS.WebSocket
       -> Proxy rqTy
       -> request
       -> (response -> IO ())
       -> IO ()
remote api ws p rq f = do
  u   <- hashUnique <$> newUnique
  u'  <- hashUnique <$> newUnique
  void $ forkIO $ void $ do
    s <- time
    r <- WS.apiRequest api ws p (u,rq) $ \_ rsp -> do
      e <- time
      logJSON (requestHeader p,rsp,e - s)
      traverse_ f rsp
    -- when debug (void $ WS.apiRequest api ws debugServer (u',()) $ \_ rsp -> traverse_ logJSON rsp)
    return r

-- Placeholder during development for future view themes.
instance Themeable () where
  theme c _ = return ()

calc :: Txt -> Txt
calc x = "calc(" <> x <> ")"

fill :: Txt
fill = "fill"
