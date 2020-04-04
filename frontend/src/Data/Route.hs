{-# language DeriveAnyClass #-}
module Data.Route where

import Pure.Router (path,dispatch)
import qualified Pure.Data.URI as URI (decodeURI)

import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Elm.Application (Routes(..),URL(..))

import Data.Maybe
import GHC.Generics

import Control.Monad.IO.Class

data Route
  = NoR
  | HomeR
  | AboutR
  | BlogR
  | TutorialsR
  | DocsR
  | PostR Txt
  | PackageR Txt -- pkg
  | VersionR Txt (Maybe Txt) -- pkg,maybe ver
  | ModuleR Txt (Maybe Txt) Txt -- pkg,maybe ver,module
  | EntityR Txt (Maybe Txt) Txt Txt -- pkg,maybe ver,module,entity
  | TutorialR Txt
  deriving (Generic,Eq,ToJSON)

instance Routes Route where
  home = NoR

  title NoR = Nothing
  title HomeR = Just "Purehs.org"
  title AboutR = Just "Purehs.org - About"
  title BlogR = Just "Purehs.org - Blog" 
  title TutorialsR = Just "Tutorials"
  title DocsR = Just "Purehs.org - Docs"
  title (PostR s) = Nothing -- manually set title
  title (PackageR p) = Just p
  title (VersionR p (Just v)) = Just $ p <> "-" <> v
  title (VersionR p _) = Just $ p <> "- latest"
  title (ModuleR p (Just v) m) = Just $ p <> "-" <> v <> "/" <> m
  title (ModuleR p _ m) = Just $ p <> "-latest/" <> m
  title (EntityR p (Just v) m e) = Just $ p <> "-" <> v <> "/" <> m <> "." <> e
  title (EntityR p _ m e) = Just $ p <> "-latest/" <> m <> "." <> e
  title (TutorialR t) = Nothing -- manually set title

  location = Internal . loc
    where
      loc NoR = ""
      loc HomeR = "/"
      loc AboutR = "/about"
      loc BlogR = "/blog"
      loc (PostR s) = "/blog/" <> s
      loc TutorialsR = "/tut"
      loc (TutorialR t) = "/tut/" <> t
      loc DocsR = "/doc"
      loc (PackageR p) = "/doc/" <> p
      loc (VersionR p v) = "/doc/" <> p <> "/" <> fromMaybe "latest" v
      loc (ModuleR p v m) = "/doc/" <> p <> "/" <> fromMaybe "latest" v <> "/" <> m
      loc (EntityR p v m e) = "/doc/" <> p <> "/" <> fromMaybe "latest" v <> "/" <> m <> "/" <> e

  routes = about >> blog >> docs >> tutorials >> dispatch HomeR
    where
      about =
        path "/about" $
          dispatch AboutR

      blog =
        path "/blog" $ do
          path "/:slug" $ do
            s <- "slug"
            dispatch $ PostR s
          dispatch BlogR

      tutorials =
        path "/tut" $ do
          path "/:slug" $ do
            s <- "slug"
            dispatch $ TutorialR s
          dispatch TutorialsR

      docs =
        path "/doc" $ do
          path "/:pkg" $ do
            p <- "pkg"
            path "/latest" $ do
              path "/:mdl" $ do
                m <- "mdl"
                path "/:ent" $ do
                  e <- "ent" 
                  dispatch $ EntityR p Nothing m e
                dispatch $ ModuleR p Nothing m
              dispatch $ VersionR p Nothing
            path "/:ver" $ do
              v <- "ver"
              path "/:mdl" $ do
                m <- "mdl"
                path "/:ent" $ do
                  e <- "ent"
                  dispatch $ EntityR p (Just v) m (URI.decodeURI e)
                dispatch $ ModuleR p (Just v) m
              dispatch $ VersionR p (Just v)
            dispatch $ PackageR p
          dispatch DocsR

