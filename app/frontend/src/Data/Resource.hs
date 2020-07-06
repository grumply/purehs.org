module Data.Resource where

import qualified App
import Data.Route
import Shared
import Shared.Author (Author,AuthorContent)
import Shared.Blog (Post,PostContent)
import Shared.Package (Package,PackageContent,Version,Module,ModuleContent)
import Shared.Page (Page,PageContent)
import Shared.Tutorial (Tutorial,TutorialContent)
import Shared.Types (Rendered)

import Pure.Elm.Application hiding (Async,Page)

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async,async,wait)

type Request rsp = Either rsp (Async rsp)

data Resource
  = NoResource

  | PageResource
    (Request (Maybe Page))
    (Request (Maybe (PageContent Rendered)))
  
  -- PackageBlogResource == BlogResource
  -- AuthorPostsResource == BlogResource
  | BlogResource 
    (Request [(Post Rendered)])

  -- PackagePostResource == PostResource
  | PostResource 
    (Request (Maybe (Post Rendered))) 
    (Request (Maybe (PostContent Rendered)))

  -- PackageTutorialsResource == TutorialsResource 
  -- AuthorTutorialsResource == TutorialsResource
  | TutorialsResource 
    (Request [(Tutorial Rendered)])

  -- PackageTutorialResource == TutorialResource
  | TutorialResource 
    (Request (Maybe (Tutorial Rendered))) 
    (Request (Maybe (TutorialContent Rendered)))

  | AuthorsResource 
    (Request [(Author Rendered)])

  | AuthorResource 
    (Request (Maybe (Author Rendered))) 
    (Request (Maybe (AuthorContent Rendered)))

  -- AuthorPackagesResource == PackagesResource
  | PackagesResource 
    (Request [Package])

  | PackageResource 
    (Request (Maybe Package)) 
    (Request (Maybe (PackageContent Rendered)))
    (Request [(Version Rendered)])

  | ModulesResource 
    (Request (Maybe Package)) 
    (Request (Maybe (Version Rendered))) 
    (Request [((Module Rendered),(ModuleContent Rendered))])

resource :: App.App => Route -> IO Resource
resource rt = do
  rsc <- load rt 
  forkIO $ description rt rsc >>= describe
  pure rsc

load :: App.App => Route -> IO Resource
load = \case
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
    <*> (App.req session Shared.getPackageContent pn)
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
