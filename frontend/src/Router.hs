module Router where

import Lenses
import Routes
import Scope
import Shared
import State

router = do

  path "/blog/:year/:month/:day/:slug" $ do
    dispatch =<< PostR <$> "year" <*> "month" <*> "day" <*> "slug"

  path "/docs/:group/:module/:function" $
    dispatch =<< DocR <$> "group" <*> "module" <*> "function"

  path "/tuts/:num/:chapter/:group/:name" $
    dispatch =<< TutorialR <$> "num" <*> "chapter" <*> "group" <*> "name"

  path "/blog" $
    dispatch BlogR

  path "/docs" $
    dispatch DocsR

  path "/tuts" $
    dispatch TutR

  dispatch HomeR
