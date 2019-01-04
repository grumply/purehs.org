module Router where

import App
import Lenses
import Routes
import Scope
import Shared
import State

import Data.Kind
import Text.Read (readMaybe)

router = do

  path "/blog/:year/:month/:day/:slug" $
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
