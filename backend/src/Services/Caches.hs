module Services.Caches where

import Data.Cached
import Services.Utils

import qualified Shared.Tutorial as Tutorial
import qualified Shared.Doc as Doc
import qualified Shared.Page as Page
import qualified Shared.Post as Post
import qualified Shared.Package as Package
import qualified Shared.Cache as Cache

import Pure.Data.JSON (encodeBS)
import Pure.Data.Time
import Pure.Data.Try as Try
import Pure.Data.Txt as Txt
import Pure.Elm (pattern H1)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Ord (Down(..))
import Data.List as List
import System.IO.Unsafe

epoch = Hours 1 0

{-# NOINLINE packages #-}
packages :: Cached [Package.Package]
packages = unsafePerformIO $ forkCache epoch $ do
  ps <- load "packages" $ \package md ->
    let (synopsis,content) = List.break h1 md
        h1 H1 = True
        h1 _ = False
        meta = Package.Meta {..}
     in Package.Package {..}
  pure (sort ps)

{-# NOINLINE rawPackage #-}
rawPackage :: Cached (Map Txt BS.ByteString)
rawPackage = unsafePerformIO $ forkCache epoch $ do
  ps <- cached packages
  pure $ Map.fromList
    [ (k,encodeBS p)
    | p <- ps
    , let
        pm = Package.meta p
        k = Package.package pm
    ]

{-# NOINLINE docs #-}
docs :: Cached [Doc.Doc]
docs = unsafePerformIO $ forkCache epoch $ do
  ds <- load "docs" $ \fn ->
    let (Txt.reverse -> version) `Dash` (Txt.reverse -> package) = Txt.reverse fn
    in Doc.Doc Doc.Meta {..}
  pure (sort ds)

{-# NOINLINE rawDoc #-}
rawDoc :: Cached (Map (Txt,Txt) BS.ByteString)
rawDoc = unsafePerformIO $ forkCache epoch $ do
  ds <- cached docs
  pure $ Map.fromList
    [ (k,encodeBS d)
    | d <- ds
    , let
        dm = Doc.meta d
        k = (Doc.package dm,Doc.version dm)
    ]

{-# NOINLINE rawDocMetas #-}
rawDocMetas :: Cached BS.ByteString
rawDocMetas = unsafePerformIO $ forkCache epoch ((encodeBS . fmap Doc.meta) <$> cached docs)

{-# NOINLINE posts #-}
posts :: Cached [Post.Post]
posts = unsafePerformIO $ forkCache epoch $ do
  ps <- load "posts" $ \fn ->
    let year `Dash` (month `Dash` (day `Dash` slug_)) = fn
        slug = Txt.replace "_" "-" slug_
        title = Txt.toTitle . Txt.replace "_" "-"  . Txt.replace "-" " " $ slug_
    in Post.Post Post.Meta {..}
  pure $ sortOn Data.Ord.Down ps

{-# NOINLINE rawPost #-}
rawPost :: Cached (Map Txt BS.ByteString)
rawPost = unsafePerformIO $ forkCache epoch $ do
  ps <- cached posts
  pure $ Map.fromList
    [ (k,encodeBS p)
    | p <- ps
    , let
        pm = Post.meta p
        k = Post.slug pm
    ]

{-# NOINLINE rawPostMetas #-}
rawPostMetas :: Cached BS.ByteString
rawPostMetas = unsafePerformIO $ forkCache epoch ((encodeBS . fmap Post.meta) <$> cached posts)

{-# NOINLINE tutorials #-}
tutorials :: Cached [Tutorial.Tutorial]
tutorials = unsafePerformIO $ forkCache epoch $ do
  ts <- load "tutorials" $ \fn ->
    let number `Dash` slug = fn
        title = Txt.toTitle slug
    in Tutorial.Tutorial Tutorial.Meta {..}
  pure (sort ts)

{-# NOINLINE rawTutorial #-}
rawTutorial :: Cached (Map Txt BS.ByteString)
rawTutorial = unsafePerformIO $ forkCache epoch $ do
  ts <- cached tutorials
  pure $ Map.fromList
    [ (k,encodeBS t)
    | t <- ts
    , let
        tm = Tutorial.meta t
        k = Tutorial.slug tm
    ]

{-# NOINLINE rawTutorialMetas #-}
rawTutorialMetas :: Cached BS.ByteString
rawTutorialMetas = unsafePerformIO $ forkCache epoch ((encodeBS . fmap Tutorial.meta) <$> cached tutorials)

{-# NOINLINE pages #-}
pages :: Cached [Page.Page]
pages = unsafePerformIO $ forkCache epoch $ do
  ps <- load "pages" $ \fn -> Page.Page (Page.Meta fn)
  pure (sort ps)

{-# NOINLINE rawPage #-}
rawPage :: Cached (Map Txt BS.ByteString)
rawPage = unsafePerformIO $ forkCache epoch $ do
  ps <- cached pages
  pure $ Map.fromList
    [ (k,encodeBS p)
    | p <- ps
    , let
        pm = Page.meta p
        k = Page.slug pm
    ]

{-# NOINLINE rawCache #-}
rawCache :: Cached BS.ByteString
rawCache = unsafePerformIO $ forkCache epoch $ do
  pkgs <- cached packages
  ps <- cached posts
  ds <- cached docs
  ts <- cached tutorials
  pgs <- cached pages
  pure $ encodeBS $
    Cache.Cache
      (Package.meta <$> pkgs)
      (Post.meta <$> ps)
      (Doc.meta <$> ds)
      (Tutorial.meta <$> ts)
      []
      []
      []
      []
      (fmap (\p -> (Page.slug (Page.meta p),Done p)) pgs)