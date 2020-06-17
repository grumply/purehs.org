{-# language DeriveAnyClass, QuasiQuotes #-}
module Data.Route where

import Shared.Types

import Pure.Router (path,dispatch)
import qualified Pure.Data.URI as URI (decodeURI,encodeURI)

import Pure.Data.Txt
import Pure.Elm.Application (Routes(..),URL(..))

data Route
  = NoR
  | HomeR

  | PageR Slug

  | BlogR
  | PostR Slug

  | TutorialsR
  | TutorialR Slug

  | AuthorsR
  | AuthorR Name
  | AuthorPostsR Name
  | AuthorTutorialsR Name
  | AuthorPackagesR Name

  | PackagesR
  | PackageR PackageName
  | PackageBlogR PackageName
  | PackagePostR PackageName Slug
  | VersionR PackageName Version
  | VersionTutorialsR PackageName Version
  | VersionTutorialR PackageName Version Slug
  | ModuleR PackageName Version ModuleName
  | EntityR PackageName Version ModuleName Txt -- pkg,maybe ver,module,entity
  deriving (Eq)

instance Routes Route where
  home = NoR

  title NoR = Nothing
  title HomeR = Just "Pure.hs"

  title (PageR s) = Just (toTxt s)

  title BlogR = Just "Blog"
  title (PostR _) = Nothing

  title TutorialsR = Just "Tutorials"
  title (TutorialR _) = Nothing

  title AuthorsR = Just "Authors"
  title (AuthorR n) = Just (toTxt n)
  title (AuthorPostsR n) = Just (toTxt n <> " - Posts")
  title (AuthorTutorialsR n) = Just (toTxt n <> " - Tutorials")
  title (AuthorPackagesR n) = Just (toTxt n <> " - Packages")

  title PackagesR = Just "Packages"
  title (PackageR p) = Just (toTxt p)
  title (PackageBlogR p) = Just (toTxt p <> " - Blog")
  title (PackagePostR p s) = Nothing
  title (VersionR p v) = Just (toTxt p <> "/" <> toTxt v <> " - Documentation")
  title (VersionTutorialsR p v) = Just (toTxt p <> "/" <> toTxt v <> " - Tutorials")
  title (VersionTutorialR p v s) = Nothing
  title (ModuleR p v m) = Just (toTxt m)
  title (EntityR p v m e) = Just (toTxt e )

  location = Internal . loc
    where
      loc NoR = ""
      loc HomeR = "/"

      loc (PageR s) = "/" <> toTxt s

      loc BlogR = "/blog"
      loc (PostR s) = "/blog/" <> toTxt s

      loc TutorialsR = "/tutorials"
      loc (TutorialR t) = "/tutorials/" <> toTxt t

      loc AuthorsR = "/authors"
      loc (AuthorR (Name a)) = "/authors/" <> URI.encodeURI a 
      loc (AuthorPostsR (Name a)) = "/authors/" <> URI.encodeURI a <> "/blog"
      loc (AuthorTutorialsR (Name a)) = "/authors/" <> URI.encodeURI a <> "/tutorials"
      loc (AuthorPackagesR (Name a)) = "/authors/" <> URI.encodeURI a <> "/packages"

      loc PackagesR = "/packages"
      loc (PackageR p) = "/packages/" <> toTxt p
      loc (PackageBlogR p) = "/packages/" <> toTxt p <> "/blog"
      loc (PackagePostR p s) = "/packages/" <> toTxt p <> "/blog/" <> toTxt s
      loc (VersionR p v) = "/packages/" <> toTxt p <> "/" <> toTxt v
      loc (VersionTutorialsR p v) = "/packages/" <> toTxt p <> "/" <> toTxt v <> "/tutorials"
      loc (VersionTutorialR p v s) = "/packages/" <> toTxt p <> "/" <> toTxt v <> "/tutorials/" <> toTxt s
      loc (ModuleR p v m) = "/packages/" <> toTxt p <> "/" <> toTxt v <> "/" <> toTxt m
      loc (EntityR p v m e) = "/packages/" <> toTxt p <> "/" <> toTxt v <> "/" <> toTxt m <> "/" <> toTxt e

  routes = do
    blog 
    tutorials 
    authors 
    packages 
    page
    dispatch HomeR
    where

      blog =
        path "/blog" $ do
          path "/:slug" $ do
            s <- "slug"
            dispatch $ PostR s
          dispatch BlogR

      page = do
        path "/:slug" $ do
          s <- "slug"
          dispatch $ 
            if s == "" 
              then HomeR 
              else PageR s

      tutorials =
        path "/tutorials" $ do
          path "/:slug" $ do
            s <- "slug"
            dispatch $ TutorialR s
          dispatch TutorialsR

      authors =
        path "/authors" $ do
          path "/:author" $ do
            a <- Name . URI.decodeURI <$> "author"
            path "/packages" $ 
              dispatch $ AuthorPackagesR a
            path "/blog" $ 
              dispatch $ AuthorPostsR a
            path "/tutorials" $ 
              dispatch $ AuthorTutorialsR a
            dispatch $ AuthorR a
          dispatch AuthorsR

      packages =
        path "/packages" $ do
          path "/:pkg" $ do
            p <- "pkg"
            path "/blog" $ do
              path "/:slug"  $ do
                s <- "slug"
                dispatch $ PackagePostR p s
              dispatch $ PackageBlogR p
            path "/:ver" $ do
              v <- "ver"
              path "/tutorials" $ do
                path "/:slug" $ do
                  s <- "slug"
                  dispatch $ VersionTutorialR p v s
                dispatch $ VersionTutorialsR p v
              path "/:mdl" $ do
                m <- "mdl"
                path "/:ent" $ do
                  e <- "ent"
                  dispatch $ EntityR p v m (URI.decodeURI e)
                dispatch $ ModuleR p v m
              dispatch $ VersionR p v
            dispatch $ PackageR p
          dispatch PackagesR

