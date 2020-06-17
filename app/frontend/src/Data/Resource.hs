module Data.Resource where

import qualified App
import Data.Route
import Shared
import Shared.Author (AuthorView,AuthorContentView)
import Shared.Blog (PostView,PostContentView)
import Shared.Package (PackageView,VersionView,ModuleView,ModuleContentView)
import Shared.Page (PageView,PageContentView)
import Shared.Tutorial (TutorialView,TutorialContentView)

import Pure.Elm.Application hiding (Async)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async,async,wait)

type Request rsp = Either rsp (Async rsp)

data Resource
  = NoResource

  | PageResource
    (Request (Maybe PageView))
    (Request (Maybe PageContentView))
  
  -- PackageBlogResource == BlogResource
  -- AuthorPostsResource == BlogResource
  | BlogResource 
    (Request [PostView])

  -- PackagePostResource == PostResource
  | PostResource 
    (Request (Maybe PostView)) 
    (Request (Maybe PostContentView))

  -- PackageTutorialsResource == TutorialsResource 
  -- AuthorTutorialsResource == TutorialsResource
  | TutorialsResource 
    (Request [TutorialView])

  -- PackageTutorialResource == TutorialResource
  | TutorialResource 
    (Request (Maybe TutorialView)) 
    (Request (Maybe TutorialContentView))

  | AuthorsResource 
    (Request [AuthorView])

  | AuthorResource 
    (Request (Maybe AuthorView)) 
    (Request (Maybe AuthorContentView))

  -- AuthorPackagesResource == PackagesResource
  | PackagesResource 
    (Request [PackageView])

  | PackageResource 
    (Request (Maybe PackageView)) 
    (Request [VersionView])

  | ModulesResource 
    (Request (Maybe PackageView)) 
    (Request (Maybe VersionView)) 
    (Request [(ModuleView,ModuleContentView)])

resource :: App.App => Route -> IO Resource
resource rt = do
  rsc <- go rt 
  forkIO $ description rt rsc >>= describe
  pure rsc
  where
    go = \case
      NoR -> pure NoResource

      HomeR -> pure NoResource

      PageR s -> PageResource
        <$> (App.req session Shared.getPage s)
        <*> (App.req session Shared.getPageContent s)

      BlogR -> BlogResource 
        <$> (App.req session Shared.listPosts ())

      PostR s -> PostResource 
        <$> (App.req session Shared.getPost s)
        <*> (App.req session Shared.getPostContent s)

      TutorialsR -> TutorialsResource 
        <$> (App.req session Shared.listTutorials ())

      TutorialR s -> TutorialResource 
        <$> (App.req session Shared.getTutorial s)
        <*> (App.req session Shared.getTutorialContent s)

      AuthorsR -> AuthorsResource
        <$> (App.req session Shared.listAuthors ())

      AuthorR n -> AuthorResource
        <$> (App.req session Shared.getAuthor n)
        <*> (App.req session Shared.getAuthorContent n)

      AuthorPostsR n -> BlogResource
        <$> (App.req session Shared.listAuthorPosts n)

      AuthorTutorialsR n -> TutorialsResource
        <$> (App.req session Shared.listAuthorTutorials n)

      AuthorPackagesR nm -> PackagesResource 
        <$> (App.req session Shared.listAuthorPackages nm)

      PackagesR -> PackagesResource 
        <$> (App.req session Shared.listPackages ())

      PackageR pn -> PackageResource 
        <$> (App.req session Shared.getPackage pn)
        <*> (App.req session Shared.listPackageVersions pn)

      PackageBlogR pn -> BlogResource
        <$> (App.req session Shared.listPackagePosts pn)

      PackagePostR pn s -> PostResource
        <$> (App.req session Shared.getPackagePost (pn,s))
        <*> (App.req session Shared.getPackagePostContent (pn,s))

      VersionTutorialsR pn v -> TutorialsResource 
        <$> (App.req session Shared.listPackageVersionTutorials (pn,v))

      VersionTutorialR pn v s -> TutorialResource 
        <$> (App.req session Shared.getPackageVersionTutorial (pn,v,s))
        <*> (App.req session Shared.getPackageVersionTutorialContent (pn,v,s))

      VersionR pn v -> ModulesResource 
        <$> (App.req session Shared.getPackage pn)
        <*> (App.req session Shared.getPackageVersion (pn,v))
        <*> (App.req session Shared.listPackageVersionModulesContent (pn,v))

      ModuleR pn v mn -> ModulesResource 
        <$> (App.req session Shared.getPackage pn)
        <*> (App.req session Shared.getPackageVersion (pn,v))
        <*> (App.req session Shared.listPackageVersionModulesContent (pn,v))

      EntityR pn v mn e -> ModulesResource 
        <$> (App.req session Shared.getPackage pn)
        <*> (App.req session Shared.getPackageVersion (pn,v))
        <*> (App.req session Shared.listPackageVersionModulesContent (pn,v))

description :: Route -> Resource -> IO Txt
description rt rsc =
  case rt of
    AuthorPackagesR n -> pure $ "Pure.hs packages authored by " <> toTxt n
    PackagesR -> pure "Pure.hs has an extensive collection of packages to help you design powerful, performant, and safe server and client applications."
    _ -> 
      case rsc of
        _ -> pure ""
