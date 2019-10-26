{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Types
import qualified View.About as View
import qualified View.Blog as View
import qualified View.Docs as View
import qualified View.Home as View
import qualified View.Tutorials as View

import Shared
import qualified Shared.Tutorial as Tut (Tutorial(..),Meta(..))
import qualified Shared.Doc as Doc (Doc(..),Meta(..))
import qualified Shared.Package as Package (Package(..),Meta(..))
import qualified Shared.Page as Page (Page(..),Meta(..))
import qualified Shared.Post as Post (Post(..),Meta(..))
import qualified Shared.Cache as Cache (Cache(..))
import Shared.Utils (asMap)

import Pure.Data.Try
import Pure.Data.URI (decodeURI)
import Pure.Elm
import qualified Pure.Router as Router
import Pure.Router (Router(..),Routing,dispatch,path)
import Pure.WebSocket ((<:>))
import qualified Pure.WebSocket as WS

import qualified Data.Map as Map

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import System.IO.Unsafe

main = inject body (run (App [Startup] [] [] model update view) ())

impl = WS.Impl Shared.clientApi msgs reqs
  where
    msgs = handleSetCache <:> WS.none
    reqs = WS.none

handleSetCache :: Elm Msg => WS.MessageHandler SetCache
handleSetCache = WS.awaiting $ do
  c <- WS.acquire
  liftIO $ command (SetCache c)

router :: Routing Route ()
router = about >> blog >> docs >> tutorials >> dispatch HomeR
  where
    about =
      path "/about" $
        dispatch AboutR

    blog =
      path "/blog" $ do
        path "/:slug" $ do
          s <- "slug"
          dispatch $ PostR s
        dispatch BlogR

    tutorials =
      path "/tut" $ do
        path "/:slug" $ do
          s <- "slug"
          dispatch $ TutorialR s
        dispatch TutorialsR

    docs =
      path "/doc" $ do
        path "/:pkg" $ do
          p <- "pkg"
          path "/:ver" $ do
            v <- "ver"
            path "/:mdl" $ do
              m <- "mdl"
              path "/:ent" $ do
                e <- "ent"
                dispatch $ EntityR p v m (decodeURI e)
              dispatch $ ModuleR p v m
            dispatch $ VersionR p v
          dispatch $ PackageR p
        dispatch DocsR

view :: Elm Msg => () -> Model -> View
view _ mdl =
  Div <||>
    [ case route mdl of
        NoR         -> Null
        HomeR       -> View.home mdl
        AboutR      -> View.about mdl
        BlogR       -> View.blog mdl
        PostR _     -> View.blog mdl
        TutorialR _ -> View.tutorials mdl
        TutorialsR  -> View.tutorials mdl
        _           -> View.docs mdl
    , View (Router NoR (Router.route Main.router >=> inject))
    ]
  where
    inject :: Maybe Route -> IO (Maybe Route)
    inject mmsg = do
      for_ mmsg (command . Route)
      pure mmsg

update :: Elm Msg => Msg -> () -> Model -> IO Model
update msg _ mdl | Cache.Cache {..} <- cache mdl = do
  let updCache f = mdl { cache = f (cache mdl) }
      setDoc p v td = updCache $ \c -> c { Cache.docs = asMap (Cache.docs c) (Map.insert (p,v) td) }
      setTut s tt   = updCache $ \c -> c { Cache.tutorials = asMap (Cache.tutorials c) (Map.insert s tt) }
      setPost s tp  = updCache $ \c -> c { Cache.posts = asMap (Cache.posts c) (Map.insert s tp) }
      setPage p tp  = updCache $ \c -> c { Cache.pages = asMap (Cache.pages c) (Map.insert p tp) }
      setPackage p tp = updCache $ \c -> c { Cache.packages = asMap (Cache.packages c) (Map.insert p tp) }
   in case msg of

        Startup -> do
          ws <- WS.websocket
          WS.enact ws impl
          WS.activate ws host port False
          subscribe
          pure mdl { client = Just ws }

        Route r -> let mdl' = mdl { route = r } in
          case r of
            PostR s | Nothing <- lookup s posts ->
              update (LoadPost s) () mdl'

            TutorialR s | Nothing <- lookup s tutorials ->
              update (LoadTutorial s) () mdl'

            _ ->
              pure mdl'

        SetCache c ->
          pure mdl { cache = c <> cache mdl }

        LoadDoc p v | Nothing <- lookup (p,v) (Cache.docs (cache mdl))
                    , Just c <- client mdl -> do
          WS.remote api c getDoc (p,v) (command . SetDoc p v)
          pure (setDoc p v Trying)

        SetDoc p v md ->
          pure (setDoc p v (maybe Failed Done md))


        LoadPackage p | Nothing <- lookup p (Cache.packages (cache mdl))
                      , Just c <- client mdl -> do
          WS.remote api c getPackage p (command . SetPackage p)
          pure (setPackage p Trying)

        SetPackage p mp ->
          pure (setPackage p (maybe Failed Done mp))

        LoadPost s | Nothing <- lookup s (Cache.posts (cache mdl))
                   , Just c <- client mdl -> do
          WS.remote api c getPost s (command . SetPost s)
          pure (setPost s Trying)

        SetPost s mp ->
          pure (setPost s (maybe Failed Done mp))

        LoadPage p | Nothing <- lookup p (Cache.pages (cache mdl))
                   , Just c <- client mdl -> do
          WS.remote api c getPage p (command . SetPage p)
          pure (setPage p Trying)

        SetPage p mp ->
          pure (setPage p (maybe Failed Done mp))

        LoadTutorial n | Nothing <- lookup n (Cache.tutorials (cache mdl))
                       , Just c <- client mdl -> do
          WS.remote api c getTutorial n (command . SetTutorial n)
          pure (setTut n Trying)

        SetTutorial n mt ->
          pure (setTut n (maybe Failed Done mt))

        _ ->
          pure mdl
