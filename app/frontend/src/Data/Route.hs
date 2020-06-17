{-# language DeriveAnyClass, QuasiQuotes #-}
module Data.Route where

import Shared.Types

import Pure.Router (path,dispatch)
import qualified Pure.Data.URI as URI (decodeURI,encodeURI)

import Pure.Data.Txt
import Pure.Data.Txt.Interpolate
import Pure.Data.Txt.Search
import Pure.Data.JSON
import Pure.Elm.Application (Routes(..),URL(..))

import GHC.Generics

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
  deriving (Generic,Eq,ToJSON,Search)

instance Routes Route where
  home = NoR

  title NoR = Nothing
  title HomeR = Just "Pure.hs"

  title (PageR s) = Just [i|#{s}|]

  title BlogR = Just "Blog"
  title (PostR _) = Nothing

  title TutorialsR = Just "Tutorials"
  title (TutorialR _) = Nothing

  title AuthorsR = Just "Authors"
  title (AuthorR n) = Just [i|#{n}|]
  title (AuthorPostsR n) = Just [i|#{n} - Posts|]
  title (AuthorTutorialsR n) = Just [i|#{n} - Tutorials|]
  title (AuthorPackagesR n) = Just [i|#{n} - Packages|]

  title PackagesR = Just "Packages"
  title (PackageR p) = Just [i|#{p}|]
  title (PackageBlogR p) = Just [i|#{p} - Blog|]
  title (PackagePostR p s) = Nothing
  title (VersionR p v) = Just [i|#{p}/#{v} - Documentation|]
  title (VersionTutorialsR p v) = Just [i|#{p}/#{v} - Tutorials|]
  title (VersionTutorialR p v s) = Nothing
  title (ModuleR p v m) = Just [i|#{m}|]
  title (EntityR p v m e) = Just [i|#{e}|]

  location = Internal . loc
    where
      loc NoR = ""
      loc HomeR = "/"

      loc (PageR s) = "/" <> toTxt s

      loc BlogR = "/blog"
      loc (PostR s) = [i|/blog/#{s}|]

      loc TutorialsR = "/tutorials"
      loc (TutorialR t) = [i|/tutorials/#{t}|]

      loc AuthorsR = "/authors"
      loc (AuthorR (Name a)) = [i|/authors/#{URI.encodeURI a}|]
      loc (AuthorPostsR (Name a)) = [i|/authors/#{URI.encodeURI a}/blog|]
      loc (AuthorTutorialsR (Name a)) = [i|/authors/#{URI.encodeURI a}/tutorials|]
      loc (AuthorPackagesR (Name a)) = [i|/authors/#{URI.encodeURI a}/packages|]

      loc PackagesR = "/packages"
      loc (PackageR p) = [i|/packages/#{p}|]
      loc (PackageBlogR p) = [i|/packages/#{p}/blog|]
      loc (PackagePostR p s) = [i|/packages/#{p}/blog/#{s}|]
      loc (VersionR p v) = [i|/packages/#{p}/#{v}|]
      loc (VersionTutorialsR p v) = [i|/packages/#{p}/#{v}/tutorials|]
      loc (VersionTutorialR p v s) = [i|/packages/#{p}/#{v}/tutorials/#{s}|]
      loc (ModuleR p v m) = [i|/packages/#{p}/#{v}/#{m}|]
      loc (EntityR p v m e) = [i|/packages/#{p}/#{v}/#{m}/#{e}|]

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

