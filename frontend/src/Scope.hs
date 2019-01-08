{-# LANGUAGE ImplicitParams, ConstraintKinds, AllowAmbiguousTypes #-}
module Scope (module Scope, module Lenses, module App, module Routes, module Shared, module State, module Services) where

import Pure (Txt)
import Pure.Cache
import Pure.Cache.DynamicMap

import App hiding (Route)
import Lenses
import Routes
import Services
import Shared
import State

import Data.Kind

type AppContext = ContextApp Route State

type StateScope = (?state :: State)
type RouteScope = (?route :: Route)
type AppScope   = (?app   :: AppRef Route State)
type PageScope  = (AppScope,RouteScope,StateScope,Caching)

type DocScope  = (PageScope,?doc  :: (Txt,Txt))
type TutScope  = (PageScope,?tut  :: Txt)
type PostScope = (PageScope,?post :: Txt)

-- TODO: I'm not especially thrilled with this approach. It was copied from gitguilds.
class Supply (scope :: Constraint) where
    type Ctx scope :: *

    -- Reify a value as a constraint.
    reify :: Ctx scope -> (scope => a) -> a

    -- Reflect a reified value.
    reflect :: scope => (Ctx scope -> a) -> a

extend :: forall scope scope' a. (Supply scope, Supply scope', scope) => (Ctx scope -> Ctx scope') -> (scope' => a) -> a
extend f a = reflect @scope $ \s -> reify @scope' (f s) a

instance Supply (?context :: Context m p s super) where
   type Ctx (?context :: Context m p s super) = Context m p s super
   reflect f = f ?context
   reify context f =
       let ?context = context
       in f

instance Supply StateScope where
    type Ctx StateScope = State
    reflect f = f ?state
    reify state f =
        let ?state = state
        in f

instance Supply RouteScope where
    type Ctx RouteScope = Route
    reflect f = f ?route
    reify route f =
        let ?route = route
        in f

instance Supply AppScope where
    type Ctx AppScope = AppRef Route State
    reflect f = f ?app
    reify app f =
        let ?app = app
        in f

instance Supply PageScope where
    type Ctx PageScope = (AppRef Route State,Route,State,Cache)
    reflect f = f (?app,?route,?state,?__dynamicCache)
    reify (app,route,state,cache) f =
        let ?app = app
            ?route = route
            ?state = state
            ?__dynamicCache = cache
        in f

instance Supply DocScope where
  type Ctx DocScope = (Ctx PageScope,(Txt,Txt))
  reflect f = reflect @PageScope $ \ps -> f (ps,?doc)
  reify (pg,dp) f =
    reify @PageScope pg $
      let ?doc = dp
      in f

instance Supply TutScope where
  type Ctx TutScope = (Ctx PageScope,Txt)
  reflect f = reflect @PageScope $ \ps -> f (ps,?tut)
  reify (pg,tp) f =
    reify @PageScope pg $
      let ?tut = tp
      in f

instance Supply PostScope where
  type Ctx PostScope = (Ctx PageScope,Txt)
  reflect f = reflect @PageScope $ \ps -> f (ps,?post)
  reify (pg,pp) f =
    reify @PageScope pg $
      let ?post = pp
      in f

withState :: StateScope => (State -> a) -> a
withState = reflect @StateScope

withRoute :: RouteScope => (Route -> a) -> a
withRoute = reflect @RouteScope

withPage :: PageScope => ((AppRef Route State,Route,State,Cache) -> a) -> a
withPage = reflect @PageScope

withDoc :: DocScope => ((Txt,Txt) -> a) -> a
withDoc f = reflect @DocScope $ \(_,dp) -> f dp

withTut :: TutScope => (Txt -> a) -> a
withTut f = reflect @TutScope $ \(_,tp) -> f tp

withPost :: PostScope => (Txt -> a) -> a
withPost f = reflect @PostScope $ \(_,pp) -> f pp

usingState :: State -> (StateScope => b) -> b
usingState = reify @StateScope

usingRoute :: Route -> (RouteScope => b) -> b
usingRoute = reify @RouteScope

usingDoc :: PageScope => (Txt,Txt) -> (DocScope => b) -> b
usingDoc dp f = withPage $ \ps -> reify @DocScope (ps,dp) f

usingTut :: PageScope => Txt -> (TutScope => b) -> b
usingTut tp f = withPage $ \ps -> reify @TutScope (ps,tp) f

usingPost :: PageScope => Txt -> (PostScope => b) -> b
usingPost pp f = withPage $ \ps -> reify @PostScope (ps,pp) f
