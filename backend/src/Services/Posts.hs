module Services.Posts (Posts(),MonadPosts(..),productionPosts) where

import Pure.Capability
import Pure.Capability.TH
import Pure.Data.Txt as Txt

import Data.IORef
import Data.List as List
import Data.Maybe
import System.IO.Unsafe

import Services.Shared
import Shared (Post(..),PostMeta(..))

{-# NOINLINE posts #-}
posts :: IORef [Post]
posts = unsafePerformIO $ newIORef []

data Posts m = Posts
  { _loadPosts :: m ()
  , _lookupPost :: Txt -> m (Maybe Post)
  , _getPostMetas :: m [PostMeta]
  }
mkCapability ''Posts

productionPosts :: forall m. MonadIO m => Posts m
productionPosts = Posts {..}
  where
    _loadPosts :: m ()
    _loadPosts = liftIO $ do
      ps <- load "posts" $ \fn ->
        let year `Dash` month `Dash` day `Dash` slug_ = fn
            slug = Txt.replace "_" "-" slug_
            title = Txt.toTitle . Txt.replace "_" "-"  . Txt.replace "-" " " $ slug_
        in Post PostMeta {..}
      -- note we store these in most-recent-first order, hence the reverse
      writeIORef posts (List.reverse $ sortOn meta ps)

    _lookupPost :: Txt -> m (Maybe Post)
    _lookupPost s = do
      ps <- liftIO (readIORef posts)
      let matches = List.filter ((s ==) . slug . meta) ps
      pure $ listToMaybe matches

    _getPostMetas :: m [PostMeta]
    _getPostMetas = do
      ps <- liftIO (readIORef posts)
      pure (fmap meta ps)


