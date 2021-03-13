{-# language NoMonomorphismRestriction #-}
module Components.Preload (preload,prelink) where

import App
import Shared
import Pure.Elm.Application
import Data.Route

prelink :: (Routes rt, HasFeatures c) => rt -> c -> c
prelink r = 
  let ref = location r
  in link r . OnMouseOver (\_ -> preload ref) . OnTouchStart (\_ -> preload ref)

preload :: URL -> IO ()
preload (External _) = pure ()
preload (Internal ref) = do
  mr <- route (routes @Route) ref
  case mr of
    Nothing -> pure ()
    Just r  -> void (load r)

load :: Route -> IO ()
load = \case
  BlogRoute r -> case r of
    BlogR -> void do
      rq listPosts ()
    PostR s -> void do
      rq getPost s
      rq getPostContent s

  TutorialRoute r -> case r of
    TutorialsR -> void do
      rq listTutorials ()
    TutorialR s -> void do
      rq getTutorial s
      rq getTutorialContent s

  AuthorRoute r -> case r of 
    AuthorsR -> void do
      rq listAuthors () 
    AuthorR a -> void do
      rq getAuthor a
      rq getAuthorContent a
    AuthorBlogR a -> void do
      rq listAuthorPosts a
    AuthorTutorialsR a -> void do
      rq listAuthorTutorials a
    AuthorPackagesR a -> void do
      rq listAuthorPackages a

  PackageRoute r -> case r of
    PackagesR -> void do
      rq listPackages ()
    PackageR p -> void do
      rq getPackage p
      rq getPackageContent p
      rq listPackageVersions p
    PackageBlogR p r -> case r of
      BlogR -> void do
        rq listPackagePosts p
      PostR s -> void do
        rq getPackagePost (p,s)
        rq getPackagePostContent (p,s)
    PackageVersionR p v -> void do
      rq getPackage p
      rq getPackageVersion (p,v)
      rq listPackageVersionModulesContent (p,v)
    PackageTutorialR p v r -> case r of
      TutorialsR -> void do
        rq listPackageVersionTutorials (p,v)
      TutorialR s -> void do
        rq getPackageVersionTutorial (p,v,s)
        rq getPackageVersionTutorialContent (p,v,s)
    PackageModuleR p v _ -> void do
      rq getPackage p
      rq getPackageVersion (p,v)
      rq listPackageVersionModulesContent (p,v)
    PackageEntityR p v _ _ -> void do
      rq getPackage p
      rq getPackageVersion (p,v)
      rq listPackageVersionModulesContent (p,v)

  _ -> pure ()
  where
    rq = App.req backend

