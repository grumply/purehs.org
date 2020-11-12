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

data Resource
  = NoResource

  | PageResource
    (Async (Maybe Page))
    (Async (Maybe (PageContent Rendered)))
  
  -- PackageBlogResource == BlogResource
  -- AuthorPostsResource == BlogResource
  | BlogResource 
    (Async [(Post Rendered)])

  -- PackagePostResource == PostResource
  | PostResource 
    (Async (Maybe (Post Rendered))) 
    (Async (Maybe (PostContent Rendered)))

  -- PackageTutorialsResource == TutorialsResource 
  -- AuthorTutorialsResource == TutorialsResource
  | TutorialsResource 
    (Async [(Tutorial Rendered)])

  -- PackageTutorialResource == TutorialResource
  | TutorialResource 
    (Async (Maybe (Tutorial Rendered))) 
    (Async (Maybe (TutorialContent Rendered)))

  | AuthorsResource 
    (Async [(Author Rendered)])

  | AuthorResource 
    (Async (Maybe (Author Rendered))) 
    (Async (Maybe (AuthorContent Rendered)))

  -- AuthorPackagesResource == PackagesResource
  | PackagesResource 
    (Async [Package])

  | PackageResource 
    (Async (Maybe Package)) 
    (Async (Maybe (PackageContent Rendered)))
    (Async [(Version Rendered)])

  | ModulesResource 
    (Async (Maybe Package)) 
    (Async (Maybe (Version Rendered))) 
    (Async [((Module Rendered),(ModuleContent Rendered))])

resource :: App.App => Route -> IO Resource
resource rt = do
  rsc <- load rt 
  forkIO $ description rt rsc >>= describe
  pure rsc

load :: Route -> IO Resource
load = \case
  NoR -> pure NoResource

  HomeR -> pure NoResource

  PageR s -> PageResource
    <$> (rq Shared.getPage s)
    <*> (rq Shared.getPageContent s)

  BlogR -> BlogResource 
    <$> (rq Shared.listPosts ())

  PostR s -> PostResource 
    <$> (rq Shared.getPost s)
    <*> (rq Shared.getPostContent s)

  TutorialsR -> TutorialsResource 
    <$> (rq Shared.listTutorials ())

  TutorialR s -> TutorialResource 
    <$> (rq Shared.getTutorial s)
    <*> (rq Shared.getTutorialContent s)

  AuthorsR -> AuthorsResource
    <$> (rq Shared.listAuthors ())

  AuthorR n -> AuthorResource
    <$> (rq Shared.getAuthor n)
    <*> (rq Shared.getAuthorContent n)

  AuthorPostsR n -> BlogResource
    <$> (rq Shared.listAuthorPosts n)

  AuthorTutorialsR n -> TutorialsResource
    <$> (rq Shared.listAuthorTutorials n)

  AuthorPackagesR nm -> PackagesResource 
    <$> (rq Shared.listAuthorPackages nm)

  PackagesR -> PackagesResource 
    <$> (rq Shared.listPackages ())

  PackageR pn -> PackageResource 
    <$> (rq Shared.getPackage pn)
    <*> (rq Shared.getPackageContent pn)
    <*> (rq Shared.listPackageVersions pn)

  PackageBlogR pn -> BlogResource
    <$> (rq Shared.listPackagePosts pn)

  PackagePostR pn s -> PostResource
    <$> (rq Shared.getPackagePost (pn,s))
    <*> (rq Shared.getPackagePostContent (pn,s))

  VersionTutorialsR pn v -> TutorialsResource 
    <$> (rq Shared.listPackageVersionTutorials (pn,v))

  VersionTutorialR pn v s -> TutorialResource 
    <$> (rq Shared.getPackageVersionTutorial (pn,v,s))
    <*> (rq Shared.getPackageVersionTutorialContent (pn,v,s))

  VersionR pn v -> ModulesResource 
    <$> (rq Shared.getPackage pn)
    <*> (rq Shared.getPackageVersion (pn,v))
    <*> (rq Shared.listPackageVersionModulesContent (pn,v))

  ModuleR pn v mn -> ModulesResource 
    <$> (rq Shared.getPackage pn)
    <*> (rq Shared.getPackageVersion (pn,v))
    <*> (rq Shared.listPackageVersionModulesContent (pn,v))

  EntityR pn v mn e -> ModulesResource 
    <$> (rq Shared.getPackage pn)
    <*> (rq Shared.getPackageVersion (pn,v))
    <*> (rq Shared.listPackageVersionModulesContent (pn,v))
  where
    rq req pl = async (App.req Shared.backend req pl)

description :: Route -> Resource -> IO Txt
description rt rsc =
  case rt of
    AuthorPackagesR n -> pure $ "Pure.hs packages authored by " <> toTxt n
    PackagesR -> pure "Pure.hs has an extensive collection of packages to help you design powerful, performant, and safe server and client applications."
    _ -> 
      case rsc of
        _ -> pure ""
