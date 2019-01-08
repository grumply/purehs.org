module Router where

import Lenses
import Routes
import Scope
import Services
import Shared
import State

router = do

  path "/blog/:slug" $ do
    dispatch =<< PostR <$> "slug"

  path "/doc/:package/:version" $
    dispatch =<< DocR <$> "package" <*> "version"

  path "/tut/:slug" $
    dispatch =<< TutR <$> "slug"

  path "/blog" $
    dispatch BlogR

  path "/docs" $
    dispatch DocsR

  path "/tuts" $
    dispatch TutsR

  path "/examples" $
    dispatch ExamplesR

  path "/reload" $ do
    liftIO $ req Shared.reloadMarkdown () (const (return ()))
    dispatch HomeR

  dispatch HomeR
