{-# language DeriveAnyClass, QuasiQuotes #-}
module Data.Route where

import Shared.Types

import Pure.Router (path,dispatch)
import qualified Pure.Data.URI as URI (decodeURI,encodeURI)

import Pure.Data.Txt
import Pure.Elm.Application (Routes(..),URL(..))

data BlogRoute
  = BlogR 
  | PostR Slug
  deriving Eq
  
data TutorialRoute
  = TutorialsR
  | TutorialR Slug
  deriving Eq

data AuthorRoute 
  = AuthorsR
  | AuthorR Name
  | AuthorBlogR Name
  | AuthorTutorialsR Name
  | AuthorPackagesR Name 
  deriving Eq
  
data PackageRoute
  = PackagesR
  | PackageR PackageName
  | PackageBlogR PackageName BlogRoute
  | PackageVersionR PackageName Version
  | PackageTutorialR PackageName Version TutorialRoute
  | PackageModuleR PackageName Version ModuleName
  | PackageEntityR PackageName Version ModuleName Txt 
  deriving Eq

data Route
  = NoRoute
  | HomeRoute
  | PageRoute Slug
  | BlogRoute BlogRoute
  | TutorialRoute TutorialRoute
  | AuthorRoute AuthorRoute
  | PackageRoute PackageRoute
  deriving Eq

instance Routes Route where
  home = NoRoute

  title = \case
    NoRoute -> Nothing

    HomeRoute -> Just "Pure.hs"

    PageRoute s -> Nothing

    BlogRoute r -> case r of
      BlogR -> Just "Blog"
      _ -> Nothing

    TutorialRoute r -> case r of
      TutorialsR -> Just "Tutorials"
      _ -> Nothing

    AuthorRoute r -> case r of 
      AuthorsR -> Just "Authors"
      AuthorR a -> Just ("Author - " <> toTxt a)
      AuthorBlogR a -> Just ("Author Blog - " <> toTxt a)
      AuthorTutorialsR a -> Just ("Author Tutorials - " <> toTxt a)
      AuthorPackagesR a -> Just ("Author Packages - " <> toTxt a)

    PackageRoute r -> case r of
      PackagesR -> Just "Packages"
      PackageR p -> Just ("Package - " <> toTxt p)
      PackageBlogR p r -> 
        case r of
          BlogR -> Just ("Package Blog - " <> toTxt p)
          _ -> Nothing
      PackageVersionR p v ->
        Just ("Package Documentation - " <> toTxt p <> "/" <> toTxt v)
      PackageTutorialR p v r ->
        case r of
          TutorialsR -> Just ("Package Tutorials - " <> toTxt p <> "/" <> toTxt v)
          _ -> Nothing
      PackageModuleR _ _ m -> Just (toTxt m)
      PackageEntityR _ _ _ e -> Just (toTxt e)

  location = Internal . loc
    where
      loc = \case
        NoRoute -> ""

        HomeRoute -> "/"

        PageRoute s -> "/" <> toTxt s

        BlogRoute r -> case r of
          BlogR -> "/blog"
          PostR s -> "/blog/" <> toTxt s

        TutorialRoute r -> case r of
          TutorialsR -> "/tutorials"
          TutorialR s -> "/tutorials/" <> toTxt s

        AuthorRoute r -> case r of 
          AuthorsR -> "/authors"
          AuthorR a -> "/authors/" <> URI.encodeURI (toTxt a)
          AuthorBlogR a -> "/authors/" <> URI.encodeURI (toTxt a) <> "/blog"
          AuthorTutorialsR a -> "/authors/" <> URI.encodeURI (toTxt a) <> "/tutorials"
          AuthorPackagesR a -> "/authors/" <> URI.encodeURI (toTxt a) <> "/packages"

        PackageRoute r -> case r of
          PackagesR -> "/packages"
          PackageR p -> "/packages/" <> toTxt p
          PackageBlogR p r -> 
            case r of
              BlogR -> "/packages/" <> toTxt p <> "/blog"
              PostR s -> "/packages/" <> toTxt p <> "/blog/" <> toTxt s
          PackageVersionR p v -> "/packages/" <> toTxt p <> "/" <> toTxt v
          PackageTutorialR p v r -> 
            case r of
              TutorialsR -> "/packages/" <> toTxt p <> "/" <> toTxt v <> "/tutorials"
              TutorialR s -> "/packages/" <> toTxt p <> "/" <> toTxt v <> "/tutorials/" <> toTxt s
          PackageModuleR p v m -> "/packages/" <> toTxt p <> "/" <> toTxt v <> "/" <> toTxt m
          PackageEntityR p v m e -> "/packages/" <> toTxt p <> "/" <> toTxt v <> "/" <> toTxt m <> "/" <> toTxt (URI.encodeURI e)

  routes = do
    blog 
    tutorials 
    authors 
    packages 
    page
    dispatch HomeRoute
    where

      blog =
        path "/blog" $ do
          let match = dispatch . BlogRoute
          path "/:slug" $ do
            s <- "slug"
            match (PostR s)
          match BlogR

      page = do
        path "/:slug" $ do
          s <- "slug"
          dispatch $ 
            if s == "" 
              then HomeRoute
              else PageRoute s

      tutorials =
        path "/tutorials" $ do
          let match = dispatch . TutorialRoute
          path "/:slug" $ do
            s <- "slug"
            match (TutorialR s)
          match TutorialsR

      authors =
        path "/authors" $ do
          let match = dispatch . AuthorRoute
          path "/:author" $ do
            a <- Name . URI.decodeURI <$> "author"
            path "/blog" (match (AuthorBlogR a))
            path "/tutorials" (match (AuthorTutorialsR a))
            path "/packages" (match (AuthorPackagesR a))
            match (AuthorR a)
          match AuthorsR

      packages =
        path "/packages" $ do
          let match = dispatch . PackageRoute
          path "/:pkg" $ do
            p <- "pkg"
            path "/blog" $ do
              path "/:slug"  $ do
                s <- "slug"
                match (PackageBlogR p (PostR s))
              match (PackageBlogR p BlogR)
            path "/:ver" $ do
              v <- "ver"
              path "/tutorials" $ do
                path "/:slug" $ do
                  s <- "slug"
                  match (PackageTutorialR p v (TutorialR s))
                match (PackageTutorialR p v TutorialsR)
              path "/:mdl" $ do
                m <- "mdl"
                path "/:ent" $ do
                  e <- "ent"
                  match (PackageEntityR p v m (URI.decodeURI e))
                match (PackageModuleR p v m)
              match (PackageVersionR p v)
            match (PackageR p)
          match PackagesR

toSpecificPost (BlogRoute _) s = BlogRoute (PostR s)
toSpecificPost (PackageRoute (PackageBlogR pn _)) s = PackageRoute (PackageBlogR pn (PostR s))
toSpecificPost rt@(PackageRoute PackageBlogR {}) _ = rt
toSpecificPost rt@(AuthorRoute AuthorBlogR {}) _ = rt
toSpecificPost _ _ = BlogRoute BlogR -- fallback

toSpecificTutorial (TutorialRoute _) s = TutorialRoute (TutorialR s)
toSpecificTutorial (AuthorRoute (AuthorTutorialsR a)) s = AuthorRoute (AuthorTutorialsR a) -- only global tutorials are shown at the moment
toSpecificTutorial rt@(PackageRoute (PackageTutorialR pn v _)) s = rt
toSpecificTutorial _ _ = TutorialRoute TutorialsR -- fallback

