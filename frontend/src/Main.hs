{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Pure.Data.JSON (logJSON)
import Pure.Data.Try
import Pure.Elm
import qualified Pure.Router as Router
import Pure.Router (Router(..),Routing,dispatch,path)
import Pure.WebSocket ((<:>))
import qualified Pure.WebSocket as WS

import Shared
import qualified Shared as Tut (Tutorial(..),TutorialMeta(..))
import qualified Shared as Doc (Doc(..),DocMeta(..))
import qualified Shared as Post (Post(..),PostMeta(..))
import qualified Shared as Cache (Cache(..))
import Types
import qualified View.About as View
import qualified View.Blog as View
import qualified View.Docs as View
import qualified View.Home as View
import qualified View.Tutorials as View

import qualified Data.Map as Map

import Control.Monad
import Data.Foldable
import Data.Traversable
import System.IO.Unsafe

{-# NOINLINE client #-}
client = unsafePerformIO $ do
  ws <- WS.websocket
  WS.enact ws impl
  WS.activate ws host port False
  pure ws

main = inject body (run (App [Startup] [] [] model update view) ())

impl = WS.Impl Shared.clientApi msgs reqs
  where
    msgs = handleSetCache <:> WS.none
    reqs = WS.none

handleSetCache :: Elm Msg => WS.MessageHandler SetCache
handleSetCache = WS.awaiting $ do
  c <- WS.acquire
  liftIO $ do
    logJSON (lookup "about" (pages c))
    command (SetCache c)

router :: Routing Route ()
router = do

  path "/blog/:slug" $ do
    s <- "slug"
    dispatch $ BlogR $ Just s

  path "/doc/:pkg/:ver" $ do
    p <- "pkg"
    v <- "ver"
    dispatch $ DocsR $ Just (p,v)

  path "/tut/:slug" $ do
    s <- "slug"
    dispatch $ TutsR $ Just s

  path "/blog" $ dispatch $ BlogR Nothing

  path "/doc" $ dispatch $ DocsR Nothing

  path "/tut" $ dispatch $ TutsR Nothing

  path "/about" $ dispatch AboutR

  dispatch HomeR

view :: Elm Msg => () -> Model -> View
view _ mdl =
  Div <||>
    [ case route mdl of
        NoR     -> Null
        HomeR   -> View.home mdl
        AboutR  -> View.about mdl
        BlogR _ -> View.blog mdl
        DocsR _ -> View.docs mdl
        TutsR _ -> View.tutorials mdl
    , View (Router NoR (Router.route Main.router >=> inject))
    ]
  where
    inject :: Maybe Route -> IO (Maybe Route)
    inject mmsg = do
      for_ mmsg (command . Route)
      pure mmsg

update :: Elm Msg => Msg -> () -> Model -> IO Model
update msg _ mdl =
  let updCache f = mdl { cache = f (cache mdl) }
      setDoc p v td = updCache $ \c -> c { Cache.docs = asMap (Cache.docs c) (Map.insert (p,v) td) }
      setTut s tt   = updCache $ \c -> c { Cache.tutorials = asMap (Cache.tutorials c) (Map.insert s tt) }
      setPost s tp  = updCache $ \c -> c { Cache.posts = asMap (Cache.posts c) (Map.insert s tp) }
   in case msg of

        Startup -> client `seq` pure mdl

        Route r | Cache {..} <- cache mdl -> let mdl' = mdl { route = r } in
          case r of
            BlogR Nothing -> do
              let load pm = command (LoadPost (Post.slug pm))
              traverse_ load postMetas
              pure mdl'

            BlogR (Just s) | Nothing <- lookup s posts ->
              update (LoadPost s) () mdl'

            DocsR Nothing -> do
              let load dm = command (LoadDoc (Doc.package dm) (Doc.version dm))
              traverse_ load docMetas
              pure mdl'

            DocsR (Just (p,v)) | Nothing <- lookup (p,v) docs ->
              update (LoadDoc p v) () mdl'

            TutsR Nothing -> do
              let load tm = command (LoadTutorial (Tut.slug tm))
              traverse_ load tutMetas
              pure mdl'

            TutsR (Just s) | Nothing <- lookup s tutorials ->
              update (LoadTutorial s) () mdl'

            _ ->
              pure mdl'

        SetCache c ->
          pure mdl { cache = c }

        LoadDoc p v -> do
          WS.remote api client getDoc (p,v) (command . SetDoc p v)
          pure (setDoc p v Trying)

        SetDoc p v md ->
          pure (setDoc p v (maybe Failed Done md))

        LoadPost s -> do
          WS.remote api client getPost s (command . SetPost s)
          pure (setPost s Trying)

        SetPost s mp ->
          pure (setPost s (maybe Failed Done mp))

        LoadTutorial n -> do
          WS.remote api client getTutorial n (command . SetTutorial n)
          pure (setTut n Trying)

        SetTutorial n mt ->
          pure (setTut n (maybe Failed Done mt))

        _ ->
          pure mdl

