module Services.Route where

import System.IO.Unsafe

import Imports hiding (currentRoute)

data R
  = HomeR
  | BlogR
  | PostR Txt
  | DocsR
  | DocR Txt Txt
  | TutsR
  | TutR Txt
  | ExamplesR

data Route m = Route
  { _getRoute :: m R
  , _setRoute :: R -> m ()
  }
mkCapability ''Route

currentRoute :: IORef R
currentRoute = unsafePerformIO $ newIORef HomeR

productionRoute :: forall m. MonadIO m => Route m
productionRoute = Route {..}
  where
    _getRoute = liftIO $ readIORef currentRoute
    _setRoute = liftIO . writeIORef currentRoute

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

  dispatch HomeR

isBlogRoute BlogR = True
isBlogRoute (PostR _) = True
isBlogRoute _ = False

isExamplesRoute ExamplesR = True
isExamplesRoute _ = False

isTutorialsRoute TutsR = True
isTutorialsRoute (TutR _) = True
isTutorialsRoute _ = False

isDocsRoute DocsR = True
isDocsRoute (DocR _ _) = True
isDocsRoute _ = False
