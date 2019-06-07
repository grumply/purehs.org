module Main where

import Context
import Imports

import Services.Client
import Services.Route as Route
import Services.Storage

import Pages.Doc (viewDoc)
import Pages.Docs (viewDocs)
import Pages.Examples (viewExamples)
import Pages.Home (viewHome)
import Pages.Post (viewPost)
import Pages.Posts (viewPosts)
import Pages.Tutorial (viewTutorial)
import Pages.Tutorials (viewTutorials)

data Env = Env

data State = State
  { _listening :: Bool
  , _route :: R
  }

newtype AppM a = AppM { runAppM :: Aspect (Ctx AppM) Env State a }
mkAspect ''AppM

viewApp :: Ctx AppM -> View
viewApp ctx = viewAppM app ctx Env (State False HomeR)

app :: AppM View
app = do
  State l r <- get
  c <- ctx >>= rebase
  unless l $ do
    go <- prepare1 $ put . State True
    liftIO $ onRoute' go
    put (State True r)
  pure $
    Div <| Height (per 100) . Width (per 100) |>
      [ View (Router HomeR (Imports.route Route.router))
      , case r of
          HomeR     -> viewHome (ffmap liftIO c)
          PostR s   -> viewPost (ffmap liftIO c) s
          BlogR     -> viewPosts (ffmap liftIO c)
          DocR p v  -> viewDoc (ffmap liftIO c) p v
          DocsR     -> viewDocs (ffmap liftIO c)
          TutR s    -> viewTutorial (ffmap liftIO c) s
          TutsR     -> viewTutorials (ffmap liftIO c)
          ExamplesR -> viewExamples (ffmap liftIO c)
      ]

main = inject body (viewApp productionCtx)


