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
    app = App [Startup] [] [] (pure mdl) update view
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

{-# NOINLINE authorEndpoints #-}
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

{-# NOINLINE handleListAuthors #-}
handleListAuthors :: RequestHandler Shared.ListAuthors
handleListAuthors = respondWithRaw $ \_ -> pure rawAuthorsList

{-# NOINLINE handleGetAuthor #-}
handleGetAuthor :: RequestHandler Shared.GetAuthor
handleGetAuthor = respondWithRaw $ \n ->
  pure $ fromMaybe "null" do
    Map.lookup n rawAuthors

{-# NOINLINE handleGetAuthorContent #-}
handleGetAuthorContent :: RequestHandler Shared.GetAuthorContent
handleGetAuthorContent = respondWithRaw $ \n ->
  pure $ fromMaybe "null" do
    Map.lookup n rawAuthorsContent

{-# NOINLINE handleListAuthorPackages #-}
handleListAuthorPackages :: RequestHandler Shared.ListAuthorPackages
handleListAuthorPackages = respondWithRaw $ \n ->
  pure $ fromMaybe "[]" do
    Map.lookup n rawAuthorPackages

{-# NOINLINE handleListAuthorPosts #-}
handleListAuthorPosts :: RequestHandler Shared.ListAuthorPosts
handleListAuthorPosts = respondWithRaw $ \n ->
  pure $ fromMaybe "[]" do
    Map.lookup n rawAuthorPosts

{-# NOINLINE handleListAuthorTutorials #-}
handleListAuthorTutorials :: RequestHandler Shared.ListAuthorTutorials
handleListAuthorTutorials = respondWithRaw $ \n ->
  pure $ fromMaybe "[]" do
    Map.lookup n rawAuthorTutorials



{-# NOINLINE pagesEndpoints #-}
pagesEndpoints :: Endpoints '[] _ '[] _
pagesEndpoints = Endpoints Shared.pagesAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPages
       <:> handleGetPage
       <:> handleGetPageContent
       <:> WS.none

{-# NOINLINE handleListPages #-}
handleListPages :: RequestHandler Shared.ListPages
handleListPages = respondWithRaw $ \_ -> pure rawPagesList

{-# NOINLINE handleGetPage #-}
handleGetPage :: RequestHandler Shared.GetPage
handleGetPage = respondWithRaw $ \s ->
  pure $ fromMaybe "null" do
    Map.lookup s rawPages

{-# NOINLINE handleGetPageContent #-}
handleGetPageContent :: RequestHandler Shared.GetPageContent
handleGetPageContent = respondWithRaw $ \s ->
  pure $ fromMaybe "null" do
    Map.lookup s rawPagesContent



{-# NOINLINE blogEndpoints #-}
blogEndpoints :: Endpoints '[] _ '[] _
blogEndpoints = Endpoints Shared.blogAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPosts
       <:> handleGetPost
       <:> handleGetPostContent
       <:> WS.none

{-# NOINLINE handleListPosts #-}
handleListPosts :: RequestHandler Shared.ListPosts
handleListPosts = respondWithRaw $ \_ -> pure rawPostsList

{-# NOINLINE handleGetPost #-}
handleGetPost :: RequestHandler Shared.GetPost
handleGetPost = respondWithRaw $ \s ->
  pure $ fromMaybe "null" do
    Map.lookup s rawPosts

{-# NOINLINE handleGetPostContent #-}
handleGetPostContent :: RequestHandler Shared.GetPostContent
handleGetPostContent = respondWithRaw $ \s ->
  pure $ fromMaybe "null" do
    Map.lookup s rawPostsContent



{-# NOINLINE tutorialEndpoints #-}
tutorialEndpoints :: Endpoints '[] _ '[] _
tutorialEndpoints = Endpoints Shared.tutorialAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListTutorials
       <:> handleGetTutorial
       <:> handleGetTutorialContent
       <:> WS.none

{-# NOINLINE handleListTutorials #-}
handleListTutorials :: RequestHandler Shared.ListTutorials
handleListTutorials = respondWithRaw $ \_ -> pure rawTutorialsList

{-# NOINLINE handleGetTutorial #-}
handleGetTutorial :: RequestHandler Shared.GetTutorial
handleGetTutorial = respondWithRaw $ \s ->
  pure $ fromMaybe "null" do
    Map.lookup s rawTutorials

{-# NOINLINE handleGetTutorialContent #-}
handleGetTutorialContent :: RequestHandler Shared.GetTutorialContent
handleGetTutorialContent = respondWithRaw $ \s ->
  pure $ fromMaybe "null" do
    Map.lookup s rawTutorialsContent



{-# NOINLINE packageEndpoints #-}
packageEndpoints :: Endpoints '[] _ '[] _
packageEndpoints = Endpoints Shared.packageAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackages
       <:> handleGetPackage
       <:> handleGetPackageContent
       <:> WS.none

{-# NOINLINE handleListPackages #-}
handleListPackages :: RequestHandler Shared.ListPackages
handleListPackages = respondWithRaw $ \_ -> pure rawPackagesList

{-# NOINLINE handleGetPackage #-}
handleGetPackage :: RequestHandler Shared.GetPackage
handleGetPackage = respondWithRaw $ \p ->
  pure $ fromMaybe "null" do
    Map.lookup p rawPackages

{-# NOINLINE handleGetPackageContent #-}
handleGetPackageContent :: RequestHandler Shared.GetPackageContent
handleGetPackageContent = respondWithRaw $ \p ->
  pure $ fromMaybe "null" do
    Map.lookup p rawPackageContents



{-# NOINLINE packageVersionEndpoints #-}
packageVersionEndpoints :: Endpoints '[] _ '[] _
packageVersionEndpoints = Endpoints Shared.packageVersionAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackageVersions
       <:> handleGetPackageVersion
       <:> WS.none

{-# NOINLINE handleListPackageVersions #-}
handleListPackageVersions :: RequestHandler Shared.ListPackageVersions
handleListPackageVersions = respondWithRaw $ \p ->
  pure $ fromMaybe "[]" do
    Map.lookup p rawPackageVersionsList

{-# NOINLINE handleGetPackageVersion #-}
handleGetPackageVersion :: RequestHandler Shared.GetPackageVersion
handleGetPackageVersion = respondWithRaw $ \(pn,v0) ->
  pure $ fromMaybe "null" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v) rawPackageVersions



{-# NOINLINE packageBlogEndpoints #-}
packageBlogEndpoints :: Endpoints '[] _ '[] _
packageBlogEndpoints = Endpoints Shared.packageBlogAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackagePosts
       <:> handleGetPackagePost
       <:> handleGetPackagePostContent
       <:> WS.none

{-# NOINLINE handleListPackagePosts #-}
handleListPackagePosts :: RequestHandler Shared.ListPackagePosts
handleListPackagePosts = respondWithRaw $ \p ->
  pure $ fromMaybe "[]" do
    Map.lookup p rawPackagePostsList

{-# NOINLINE handleGetPackagePost #-}
handleGetPackagePost :: RequestHandler Shared.GetPackagePost
handleGetPackagePost = respondWithRaw $ \ps ->
  pure $ fromMaybe "null" do
    Map.lookup ps rawPackagePosts

{-# NOINLINE handleGetPackagePostContent #-}
handleGetPackagePostContent :: RequestHandler Shared.GetPackagePostContent
handleGetPackagePostContent = respondWithRaw $ \ps ->
  pure $ fromMaybe "null" do
    Map.lookup ps rawPackagePostsContent



{-# NOINLINE packageTutorialEndpoints #-}
packageTutorialEndpoints :: Endpoints '[] _ '[] _
packageTutorialEndpoints = Endpoints Shared.packageTutorialAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackageVersionTutorials
       <:> handleGetPackageVersionTutorial
       <:> handleGetPackageVersionTutorialContent
       <:> WS.none

normalizeVersion :: PackageName -> Types.Version -> Maybe Types.Version
normalizeVersion pn v
  | Version "latest" <- v
  = fmap (maximum . fmap (Package.version :: Package.Version Rendered -> Types.Version)) (Map.lookup pn packageVersionsList)

  | otherwise 
  = Just v

{-# NOINLINE handleListPackageVersionTutorials #-}
handleListPackageVersionTutorials :: RequestHandler Shared.ListPackageVersionTutorials
handleListPackageVersionTutorials = respondWithRaw $ \pv@(pn,v0) ->
  pure $ fromMaybe "[]" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v) rawPackageTutorialsList

{-# NOINLINE handleGetPackageVersionTutorial #-}
handleGetPackageVersionTutorial :: RequestHandler Shared.GetPackageVersionTutorial
handleGetPackageVersionTutorial = respondWithRaw $ \(pn,v0,s) ->
  pure $ fromMaybe "null" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v,s) rawPackageTutorials

{-# NOINLINE handleGetPackageVersionTutorialContent #-}
handleGetPackageVersionTutorialContent :: RequestHandler Shared.GetPackageVersionTutorialContent
handleGetPackageVersionTutorialContent = respondWithRaw $ \(pn,v0,s) ->
  pure $ fromMaybe "null" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v,s) rawPackageTutorialsContent


{-# NOINLINE packageModuleEndpoints #-}
packageModuleEndpoints :: Endpoints '[] _ '[] _
packageModuleEndpoints = Endpoints Shared.packageModuleAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleListPackageVersionModules
       <:> handleListPackageVersionModulesContent
       <:> handleGetPackageVersionModule
       <:> handleGetPackageVersionModuleContent
       <:> WS.none

{-# NOINLINE handleListPackageVersionModules #-}
handleListPackageVersionModules :: RequestHandler Shared.ListPackageVersionModules
handleListPackageVersionModules = respondWithRaw $ \(pn,v0) ->
  pure $ fromMaybe "[]" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v) rawModulesList

{-# NOINLINE handleListPackageVersionModulesContent #-}
handleListPackageVersionModulesContent :: RequestHandler Shared.ListPackageVersionModulesContent
handleListPackageVersionModulesContent = respondWithRaw $ \(pn,v0) ->
  pure $ fromMaybe "[]" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v) rawModulesContentList

{-# NOINLINE handleGetPackageVersionModule #-}
handleGetPackageVersionModule :: RequestHandler Shared.GetPackageVersionModule
handleGetPackageVersionModule = respondWithRaw $ \(pn,v0,mn) ->
  pure $ fromMaybe "null" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v,mn) rawModules

{-# NOINLINE handleGetPackageVersionModuleContent #-}
handleGetPackageVersionModuleContent :: RequestHandler Shared.GetPackageVersionModuleContent
handleGetPackageVersionModuleContent = respondWithRaw $ \(pn,v0,mn) ->
  pure $ fromMaybe "null" do
    v <- normalizeVersion pn v0
    Map.lookup (pn,v,mn) rawModulesContent
