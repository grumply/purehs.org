module Update where

import Pure.Data.Try
import Pure.Elm
import Pure.WebSocket (remote)

import Client
import Shared
import qualified Shared as Tut (Tutorial(..),TutorialMeta(..))
import qualified Shared as Doc (Doc(..),DocMeta(..))
import qualified Shared as Post (Post(..),PostMeta(..))
import Types

import qualified Data.Map as Map

import Data.Foldable

update :: Elm Msg => Msg -> Model -> IO Model
update msg model = 
  let updCache model f = model { cache = f (cache model) } 
   in case msg of
        Route r ->
          let model' = model { route = r }
           in case r of
                BlogR Nothing -> do
                  for_ (postMetas (cache model)) $ \pm ->
                    case lookup (Post.slug pm) (posts (cache model)) of
                      Just _ -> pure ()
                      Nothing -> 
                        remote api client getPost (Post.slug pm) 
                          (command . SetPost (Post.slug pm))
                  pure model'
                BlogR (Just s) ->
                  case lookup s (posts (cache model)) of
                    Nothing -> update (LoadPost s) model'
                    _       -> pure model'
                DocsR Nothing -> do
                  for_ (docMetas (cache model)) $ \dm ->
                    case lookup (package dm,version dm) (docs (cache model)) of
                      Just _ -> pure ()
                      Nothing ->
                        remote api client getDoc (package dm,version dm)
                          (command . SetDoc (package dm) (version dm))
                  pure model'
                DocsR (Just (p,v)) ->
                  case lookup (p,v) (docs (cache model)) of
                    Nothing -> update (LoadDoc p v) model'
                    _       -> pure model'
                TutsR Nothing -> do
                  for_ (tutMetas (cache model)) $ \tm ->
                    case lookup (Tut.slug tm) (tutorials (cache model)) of
                      Just _ -> pure ()
                      Nothing ->
                        remote api client getTutorial (Tut.slug tm)
                          (command . SetTutorial (Tut.slug tm))
                  pure model'
                TutsR (Just s) ->
                  case lookup s (tutorials (cache model)) of
                    Nothing -> update (LoadTutorial s) model'
                    _       -> pure model'
                _ -> pure model'

        SetCache c -> 
          pure model { cache = c }

        LoadDoc p v -> do
          remote api client getDoc (p,v) (command . SetDoc p v)
          pure $ updCache model $ \cache -> 
            cache { docs = asMap (docs cache) (Map.insert (p,v) Trying) }

        SetDoc p v md ->
          pure $ updCache model $ \cache -> 
            cache { docs = asMap (docs cache) (Map.insert (p,v) (maybe Failed Done md)) }

        LoadPost s -> do
          remote api client getPost s (command . SetPost s)
          pure $ updCache model $ \cache ->
            cache { posts = asMap (posts cache) (Map.insert s Trying) }

        SetPost s mp ->
          pure $ updCache model $ \cache ->
            cache { posts = asMap (posts cache) (Map.insert s (maybe Failed Done mp)) }

        LoadTutorial n -> do
          remote api client getTutorial n (command . SetTutorial n)
          pure $ updCache model $ \cache ->
            cache { tutorials = asMap (tutorials cache) (Map.insert n Trying) }

        SetTutorial n mt -> do
          pure $ updCache model $ \cache ->
            cache { tutorials = asMap (tutorials cache) (Map.insert n (maybe Failed Done mt)) }


