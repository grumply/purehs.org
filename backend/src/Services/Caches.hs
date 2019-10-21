module Services.Caches where

import Data.Cached
import Services.Utils

import Shared
  (Tutorial(Tutorial),TutorialMeta(TutorialMeta)
  ,Doc(Doc),DocMeta(DocMeta)
  ,Page(Page),PageMeta(PageMeta)
  ,Post(Post),PostMeta(PostMeta)
  )
import qualified Shared as Tut (Tutorial(..),TutorialMeta(..))
import qualified Shared as Doc (Doc(..),DocMeta(..))
import qualified Shared as Page (Page(..),PageMeta(..))
import qualified Shared as Post (Post(..),PostMeta(..))
import qualified Shared as Cache (Cache(Cache))

import Pure.Data.JSON (encodeBS)
import Pure.Data.Time
import Pure.Data.Try as Try
import Pure.Data.Txt as Txt

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Map (Map)

import Data.List as List
import System.IO.Unsafe

{-# NOINLINE docs #-}
docs :: Cached [Doc]
docs = unsafePerformIO $ forkCache (Minutes 10 0) $ do
  ds <- load "docs" $ \fn ->
    let (Txt.reverse -> version) `Dash` (Txt.reverse -> package) = Txt.reverse fn
    in Doc.Doc Doc.DocMeta {..}
  pure (sortOn Doc.meta ds)

{-# NOINLINE rawDoc #-}
rawDoc :: Cached (Map (Txt,Txt) BS.ByteString)
rawDoc = unsafePerformIO $ forkCache (Minutes 10 0) $ do
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
rawDocMetas = unsafePerformIO $ forkCache (Minutes 10 0) ((encodeBS . fmap Doc.meta) <$> cached docs)

{-# NOINLINE posts #-}
posts :: Cached [Post]
posts = unsafePerformIO $ forkCache (Minutes 10 0) $ do
  ps <- load "posts" $ \fn ->
    let year `Dash` month `Dash` day `Dash` slug_ = fn
        slug = Txt.replace "_" "-" slug_
        title = Txt.toTitle . Txt.replace "_" "-"  . Txt.replace "-" " " $ slug_
    in Post.Post Post.PostMeta {..}
  pure (List.reverse $ sortOn Post.meta ps)

{-# NOINLINE rawPost #-}
rawPost :: Cached (Map Txt BS.ByteString)
rawPost = unsafePerformIO $ forkCache (Minutes 10 0) $ do
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
rawPostMetas = unsafePerformIO $ forkCache (Minutes 10 0) ((encodeBS . fmap Post.meta) <$> cached posts)

{-# NOINLINE tutorials #-}
tutorials :: Cached [Tutorial]
tutorials = unsafePerformIO $ forkCache (Minutes 10 0) $ do
  ts <- load "tutorials" $ \fn ->
    let number `Dash` slug = fn
        title = Txt.toTitle slug
    in Tut.Tutorial Tut.TutorialMeta {..}
  pure (sortOn Tut.meta ts)

{-# NOINLINE rawTutorial #-}
rawTutorial :: Cached (Map Txt BS.ByteString)
rawTutorial = unsafePerformIO $ forkCache (Minutes 10 0) $ do
  ts <- cached tutorials
  pure $ Map.fromList
    [ (k,encodeBS t)
    | t <- ts
    , let
        tm = Tut.meta t
        k = Tut.slug tm
    ]

{-# NOINLINE rawTutorialMetas #-}
rawTutorialMetas :: Cached BS.ByteString
rawTutorialMetas = unsafePerformIO $ forkCache (Minutes 10 0) ((encodeBS . fmap Tut.meta) <$> cached tutorials)

{-# NOINLINE pages #-}
pages :: Cached [Page]
pages = unsafePerformIO $ forkCache (Minutes 10 0) $ do
  ps <- load "pages" $ \fn -> Page (PageMeta fn)
  pure (sortOn Page.meta ps)

{-# NOINLINE rawPage #-}
rawPage :: Cached (Map Txt BS.ByteString)
rawPage = unsafePerformIO $ forkCache (Minutes 10 0) $ do
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
rawCache = unsafePerformIO $ forkCache (Minutes 10 0) $ do
  ps <- cached posts
  ds <- cached docs
  ts <- cached tutorials
  pgs <- cached pages
  pure $ encodeBS $
    Cache.Cache
      (Post.meta <$> ps)
      (Doc.meta <$> ds)
      (Tut.meta <$> ts)
      []
      []
      []
      (fmap (\p -> (Page.slug (Page.meta p),Done p)) pgs)