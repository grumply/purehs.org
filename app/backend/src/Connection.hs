module Connection where

import Pure.Cached
import Services.Caches

import Shared
import Shared.Types
import qualified Shared.Package as Package
import qualified Shared.Types as Types

import Pure.Elm
import Pure.WebSocket as WS

import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

data Model = Model

data Msg = Startup

connection :: WebSocket -> View
connection = run app
  where
    app = App [Startup] [] [] mdl update view
    mdl = Model
    view _ _ = Null

update :: Elm Msg => Msg -> WebSocket -> Model -> IO Model
update msg ws mdl =
  case msg of
    Startup -> do
      enact ws authorEndpoints
      enact ws pagesEndpoints
      enact ws blogEndpoints
      enact ws tutorialEndpoints
      enact ws packageEndpoints
      enact ws packageVersionEndpoints
      enact ws packageBlogEndpoints
      enact ws packageTutorialEndpoints
      enact ws packageModuleEndpoints
      activate ws
      -- c <- cached rawCache
      -- sendRaw ws (buildEncodedDispatchByteString (messageHeader setCache) c)
      pure mdl

authorEndpoints :: Endpoints '[] _ '[] _
authorEndpoints = Endpoints Shared.authorAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListAuthors
       <:> handleGetAuthor
       <:> handleGetAuthorContent
       <:> handleListAuthorPackages
       <:> handleListAuthorPosts
       <:> handleListAuthorTutorials
       <:> WS.none

handleListAuthors :: RequestHandler Shared.ListAuthors
handleListAuthors = respondWithRaw $ \_ -> do
  cached rawAuthorsList

handleGetAuthor :: RequestHandler Shared.GetAuthor
handleGetAuthor = respondWithRaw $ \n -> do
  as <- cached rawAuthors
  pure $ fromMaybe "null" (Map.lookup n as) 

handleGetAuthorContent :: RequestHandler Shared.GetAuthorContent
handleGetAuthorContent = respondWithRaw $ \n -> do
  as <- cached rawAuthorsContent
  pure $ fromMaybe "null" (Map.lookup n as)

handleListAuthorPackages :: RequestHandler Shared.ListAuthorPackages
handleListAuthorPackages = respondWithRaw $ \n -> do
  as <- cached rawAuthorPackages
  pure $ fromMaybe "[]" (Map.lookup n as)

handleListAuthorPosts :: RequestHandler Shared.ListAuthorPosts
handleListAuthorPosts = respondWithRaw $ \n -> do
  ps <- cached rawAuthorPosts
  pure $ fromMaybe "[]" (Map.lookup n ps)

handleListAuthorTutorials :: RequestHandler Shared.ListAuthorTutorials
handleListAuthorTutorials = respondWithRaw $ \n -> do
  ps <- cached rawAuthorTutorials
  pure $ fromMaybe "[]" (Map.lookup n ps)



pagesEndpoints :: Endpoints '[] _ '[] _
pagesEndpoints = Endpoints Shared.pagesAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPages
       <:> handleGetPage
       <:> handleGetPageContent
       <:> WS.none

handleListPages :: RequestHandler Shared.ListPages
handleListPages = respondWithRaw $ \_ -> do
  cached rawPagesList

handleGetPage :: RequestHandler Shared.GetPage
handleGetPage = respondWithRaw $ \s -> do
  ps <- cached rawPages
  pure $ fromMaybe "null" (Map.lookup s ps)

handleGetPageContent :: RequestHandler Shared.GetPageContent
handleGetPageContent = respondWithRaw $ \s -> do
  ps <- cached rawPagesContent
  pure $ fromMaybe "null" (Map.lookup s ps)



blogEndpoints :: Endpoints '[] _ '[] _
blogEndpoints = Endpoints Shared.blogAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPosts
       <:> handleGetPost
       <:> handleGetPostContent
       <:> WS.none

handleListPosts :: RequestHandler Shared.ListPosts
handleListPosts = respondWithRaw $ \_ -> do
  cached rawPostsList

handleGetPost :: RequestHandler Shared.GetPost
handleGetPost = respondWithRaw $ \s -> do
  ps <- cached rawPosts
  pure $ fromMaybe "null" (Map.lookup s ps)

handleGetPostContent :: RequestHandler Shared.GetPostContent
handleGetPostContent = respondWithRaw $ \s -> do
  ps <- cached rawPostsContent
  pure $ fromMaybe "null" (Map.lookup s ps)



tutorialEndpoints :: Endpoints '[] _ '[] _
tutorialEndpoints = Endpoints Shared.tutorialAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListTutorials
       <:> handleGetTutorial
       <:> handleGetTutorialContent
       <:> WS.none

handleListTutorials :: RequestHandler Shared.ListTutorials
handleListTutorials = respondWithRaw $ \_ -> do
  cached rawTutorialsList

handleGetTutorial :: RequestHandler Shared.GetTutorial
handleGetTutorial = respondWithRaw $ \s -> do
  ts <- cached rawTutorials
  pure $ fromMaybe "null" (Map.lookup s ts)

handleGetTutorialContent :: RequestHandler Shared.GetTutorialContent
handleGetTutorialContent = respondWithRaw $ \s -> do
  ts <- cached rawTutorialsContent
  pure $ fromMaybe "null" (Map.lookup s ts)



packageEndpoints :: Endpoints '[] _ '[] _
packageEndpoints = Endpoints Shared.packageAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackages
       <:> handleGetPackage
       <:> handleGetPackageContent
       <:> WS.none

handleListPackages :: RequestHandler Shared.ListPackages
handleListPackages = respondWithRaw $ \_ -> do
  cached rawPackagesList

handleGetPackage :: RequestHandler Shared.GetPackage
handleGetPackage = respondWithRaw $ \p -> do
  ps <- cached rawPackages
  pure $ fromMaybe "null" (Map.lookup p ps)

handleGetPackageContent :: RequestHandler Shared.GetPackageContent
handleGetPackageContent = respondWithRaw $ \p -> do
  ps <- cached rawPackageContents
  pure $ fromMaybe "null" (Map.lookup p ps)

packageVersionEndpoints :: Endpoints '[] _ '[] _
packageVersionEndpoints = Endpoints Shared.packageVersionAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackageVersions
       <:> handleGetPackageVersion
       <:> WS.none

handleListPackageVersions :: RequestHandler Shared.ListPackageVersions
handleListPackageVersions = respondWithRaw $ \p -> do
  ps <- cached rawPackageVersionsList
  pure $ fromMaybe "[]" (Map.lookup p ps)

handleGetPackageVersion :: RequestHandler Shared.GetPackageVersion
handleGetPackageVersion = respondWithRaw $ \(pn,v0) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "null"
    Just v  -> do
      pvs <- cached rawPackageVersions
      pure $ fromMaybe "null" (Map.lookup (pn,v) pvs)



packageBlogEndpoints :: Endpoints '[] _ '[] _
packageBlogEndpoints = Endpoints Shared.packageBlogAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackagePosts
       <:> handleGetPackagePost
       <:> handleGetPackagePostContent
       <:> WS.none

handleListPackagePosts :: RequestHandler Shared.ListPackagePosts
handleListPackagePosts = respondWithRaw $ \p -> do
  ps <- cached rawPackagePostsList
  pure $ fromMaybe "[]" (Map.lookup p ps)

handleGetPackagePost :: RequestHandler Shared.GetPackagePost
handleGetPackagePost = respondWithRaw $ \ps -> do
  pps <- cached rawPackagePosts
  pure $ fromMaybe "null" (Map.lookup ps pps)

handleGetPackagePostContent :: RequestHandler Shared.GetPackagePostContent
handleGetPackagePostContent = respondWithRaw $ \ps -> do
  ppc <- cached rawPackagePostsContent
  pure $ fromMaybe "null" (Map.lookup ps ppc)



packageTutorialEndpoints :: Endpoints '[] _ '[] _
packageTutorialEndpoints = Endpoints Shared.packageTutorialAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackageVersionTutorials
       <:> handleGetPackageVersionTutorial
       <:> handleGetPackageVersionTutorialContent
       <:> WS.none

normalizeVersion :: PackageName -> Types.Version -> IO (Maybe Types.Version)
normalizeVersion pn v = do
  case v of
    Version "latest" -> do
      pvs <- cached packageVersionsList
      pure $ fmap (maximum . fmap (Package.version :: Package.Version Rendered -> Types.Version)) (Map.lookup pn pvs)
    _ -> 
      pure (Just v)

handleListPackageVersionTutorials :: RequestHandler Shared.ListPackageVersionTutorials
handleListPackageVersionTutorials = respondWithRaw $ \pv@(pn,v0) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "[]"
    Just v  -> do
      pts <- cached rawPackageTutorialsList
      pure $ fromMaybe "[]" (Map.lookup (pn,v) pts)

handleGetPackageVersionTutorial :: RequestHandler Shared.GetPackageVersionTutorial
handleGetPackageVersionTutorial = respondWithRaw $ \(pn,v0,s) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "null"
    Just v  -> do
      pvts <- cached rawPackageTutorials
      pure $ fromMaybe "null" (Map.lookup (pn,v,s) pvts)

handleGetPackageVersionTutorialContent :: RequestHandler Shared.GetPackageVersionTutorialContent
handleGetPackageVersionTutorialContent = respondWithRaw $ \(pn,v0,s) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "null"
    Just v  -> do
      pvtcs <- cached rawPackageTutorialsContent
      pure $ fromMaybe "null" (Map.lookup (pn,v,s) pvtcs)



packageModuleEndpoints :: Endpoints '[] _ '[] _
packageModuleEndpoints = Endpoints Shared.packageModuleAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackageVersionModules
       <:> handleListPackageVersionModulesContent
       <:> handleGetPackageVersionModule
       <:> handleGetPackageVersionModuleContent
       <:> WS.none

handleListPackageVersionModules :: RequestHandler Shared.ListPackageVersionModules
handleListPackageVersionModules = respondWithRaw $ \(pn,v0) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "[]"
    Just v  -> do
      pvms <- cached rawModulesList
      pure $ fromMaybe "[]" (Map.lookup (pn,v) pvms)

handleListPackageVersionModulesContent :: RequestHandler Shared.ListPackageVersionModulesContent
handleListPackageVersionModulesContent = respondWithRaw $ \(pn,v0) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "[]"
    Just v  -> do
      pvms <- cached rawModulesContentList
      pure $ fromMaybe "[]" (Map.lookup (pn,v) pvms)

handleGetPackageVersionModule :: RequestHandler Shared.GetPackageVersionModule
handleGetPackageVersionModule = respondWithRaw $ \(pn,v0,mn) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "null"
    Just v  -> do
      pvms <- cached rawModules
      pure $ fromMaybe "null" (Map.lookup (pn,v,mn) pvms)

handleGetPackageVersionModuleContent :: RequestHandler Shared.GetPackageVersionModuleContent
handleGetPackageVersionModuleContent = respondWithRaw $ \(pn,v0,mn) -> do
  mv <- normalizeVersion pn v0
  case mv of
    Nothing -> pure "null"
    Just v  -> do
      pvms <- cached rawModulesContent
      pure $ fromMaybe "null" (Map.lookup (pn,v,mn) pvms)
