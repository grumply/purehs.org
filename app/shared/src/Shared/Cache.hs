{-# language DeriveAnyClass #-}
module Shared.Cache where

import Shared.Doc as Doc (Doc,Meta)
import Shared.Package as Pkg (Package,Meta)
import Shared.Page as Page (Page,Meta)
import Shared.Post as Post (Post,Meta)
import Shared.Tutorial as Tut (Tutorial,Meta)
import Shared.Utils (unionAsSet,unionAsMap)

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Try (Try)
import Pure.Data.Txt (Txt)

import Data.Map as Map (Map,toList,fromList,union)
import Data.Set as Set (Set,toList,fromList,union)

import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import GHC.Generics (Generic)

data Cache = Cache
  { packageMetas :: ![Pkg.Meta]
  , postMetas    :: ![Post.Meta]
  , docMetas     :: ![Doc.Meta]
  , tutMetas     :: ![Tut.Meta]
  , packages     :: ![(Txt,Try Package)]
  , posts        :: ![(Txt,Try Post)]
  , docs         :: ![((Txt,Txt),Try Doc)]
  , tutorials    :: ![(Txt,Try Tutorial)]
  , pages        :: ![(Txt,Try Page)]
  } deriving (Generic,ToJSON,FromJSON)

instance Semigroup Cache where
  (<>) l r =
    let
      s :: forall a. Ord a => (Cache -> [a]) -> [a]
      s f = unionAsSet (f l) (f r)
      m :: forall a b. Ord a => (Cache -> [(a,b)]) -> [(a,b)]
      m f = unionAsMap (f l) (f r)
    in
      Cache (s packageMetas) (s postMetas) (s docMetas) (s tutMetas)
            (m packages)     (m posts)     (m docs)     (m tutorials) (m pages)

instance Monoid Cache where
  mempty = Cache [] [] [] [] [] [] [] [] []

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' t a = Traversal t t a a

traverseTutorials :: Traversal' Cache (Map.Map Txt (Try Tutorial))
traverseTutorials f c = (\ts -> c { tutorials = Map.toList ts }) <$> f (Map.fromList $ tutorials c)

traverseDocs :: Traversal' Cache (Map.Map (Txt,Txt) (Try Doc))
traverseDocs f c = (\ds -> c { docs = Map.toList ds }) <$> f (Map.fromList $ docs c)

traversePages :: Traversal' Cache (Map.Map Txt (Try Page))
traversePages f c = (\ps -> c { pages = Map.toList ps }) <$> f (Map.fromList $ pages c)

traversePosts :: Traversal' Cache (Map.Map Txt (Try Post))
traversePosts f c = (\ps -> c { posts = Map.toList ps }) <$> f (Map.fromList $ posts c)

traversePackages :: Traversal' Cache (Map.Map Txt (Try Package))
traversePackages f c = (\ps -> c { packages = Map.toList ps }) <$> f (Map.fromList $ packages c)



