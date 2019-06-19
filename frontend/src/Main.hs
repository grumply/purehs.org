{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

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
client = unsafePerformIO (WS.clientWS host port)

main = inject body (run (App model update view) Startup)

impl = WS.Impl Shared.clientApi msgs reqs
  where
    msgs = handleSetCache
       <:> WS.none

    reqs = WS.none

handleSetCache = WS.awaiting $ do
  c <- WS.acquire
  liftIO $ command (SetCache c)

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

  path "/docs" $ dispatch $ DocsR Nothing

  path "/tuts" $ dispatch $ TutsR Nothing

  path "/about" $ dispatch AboutR

  dispatch HomeR

view :: Elm Msg => Model -> View
view model =
  Div <||>
    [ case route model of
        NoR     -> Null
        HomeR   -> View.home model
        AboutR  -> View.about model
        BlogR _ -> View.blog model
        DocsR _ -> View.docs model
        TutsR _ -> View.tutorials model
    , View (Router NoR (Router.route Main.router >=> inject))
    ]
  where
    inject :: Maybe Route -> IO (Maybe Route)
    inject mmsg = do
      for_ mmsg (command . Route)
      pure mmsg

update :: Elm Msg => Msg -> Model -> IO Model
update msg model = 
  let updCache model f = model { cache = f (cache model) } 
   in case msg of

        Startup -> do
          WS.enact client impl
          pure model

        Route r ->
          let model' = model { route = r }
           in case r of
                BlogR Nothing -> do
                  for_ (Cache.postMetas (cache model)) $ \pm ->
                    case lookup (Post.slug pm) (Cache.posts (cache model)) of
                      Just _ -> pure ()
                      Nothing -> 
                        WS.remote api client getPost (Post.slug pm) 
                          (command . SetPost (Post.slug pm))
                  pure model'

                BlogR (Just s) ->
                  case lookup s (Cache.posts (cache model)) of
                    Nothing -> update (LoadPost s) model'
                    _       -> pure model'

                DocsR Nothing -> do
                  for_ (Cache.docMetas (cache model)) $ \dm ->
                    case lookup (package dm,version dm) (Cache.docs (cache model)) of
                      Just _ -> pure ()
                      Nothing ->
                        WS.remote api client getDoc (package dm,version dm)
                          (command . SetDoc (package dm) (version dm))
                  pure model'

                DocsR (Just (p,v)) ->
                  case lookup (p,v) (Cache.docs (cache model)) of
                    Nothing -> update (LoadDoc p v) model'
                    _       -> pure model'

                TutsR Nothing -> do
                  for_ (Cache.tutMetas (cache model)) $ \tm ->
                    case lookup (Tut.slug tm) (Cache.tutorials (cache model)) of
                      Just _ -> pure ()
                      Nothing ->
                        WS.remote api client getTutorial (Tut.slug tm)
                          (command . SetTutorial (Tut.slug tm))
                  pure model'

                TutsR (Just s) ->
                  case lookup s (Cache.tutorials (cache model)) of
                    Nothing -> update (LoadTutorial s) model'
                    _       -> pure model'
                _ -> pure model'

        SetCache c -> 
          pure model { cache = c }

        LoadDoc p v -> do
          WS.remote api client getDoc (p,v) (command . SetDoc p v)
          pure $ updCache model $ \cache -> 
            cache { Cache.docs = asMap (Cache.docs cache) (Map.insert (p,v) Trying) }

        SetDoc p v md ->
          pure $ updCache model $ \cache -> 
            cache { Cache.docs = asMap (Cache.docs cache) (Map.insert (p,v) (maybe Failed Done md)) }

        LoadPost s -> do
          WS.remote api client getPost s (command . SetPost s)
          pure $ updCache model $ \cache ->
            cache { Cache.posts = asMap (Cache.posts cache) (Map.insert s Trying) }

        SetPost s mp ->
          pure $ updCache model $ \cache ->
            cache { Cache.posts = asMap (Cache.posts cache) (Map.insert s (maybe Failed Done mp)) }

        LoadTutorial n -> do
          WS.remote api client getTutorial n (command . SetTutorial n)
          pure $ updCache model $ \cache ->
            cache { Cache.tutorials = asMap (Cache.tutorials cache) (Map.insert n Trying) }

        SetTutorial n mt -> do
          pure $ updCache model $ \cache ->
            cache { Cache.tutorials = asMap (Cache.tutorials cache) (Map.insert n (maybe Failed Done mt)) }


