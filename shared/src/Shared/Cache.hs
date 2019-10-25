module Shared.Cache where

import Shared.Doc (Doc,DocMeta)
import Shared.Package (Package)
import Shared.Page (Page,PageMeta)
import Shared.Post (Post,PostMeta)
import Shared.Tutorial (Tutorial,TutorialMeta)

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Try (Try)
import Pure.Data.Txt (Txt)

import Data.Map as Map (Map,toList,fromList,union)
import Data.Set as Set (Set,toList,fromList,union)

import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import GHC.Generics (Generic)

data Cache = Cache
  { packages  :: ![Package]
  , postMetas :: ![PostMeta]
  , docMetas  :: ![DocMeta]
  , tutMetas  :: ![TutorialMeta]
  , posts     :: ![(Txt,Try Post)]
  , docs      :: ![((Txt,Txt),Try Doc)]
  , tutorials :: ![(Txt,Try Tutorial)]
  , pages     :: ![(Txt,Try Page)]
  } deriving (Generic,ToJSON,FromJSON)

instance Semigroup Cache where
  (<>) l r =
    let
      s :: forall a. Ord a => (Cache -> [a]) -> [a]
      s f = Set.toList $ Set.union (Set.fromList (f l)) (Set.fromList (f r))
      m :: forall a b. Ord a => (Cache -> [(a,b)]) -> [(a,b)]
      m f = Map.toList $ Map.union (Map.fromList (f l)) (Map.fromList (f r))
    in
      Cache (s packages) (s postMetas) (s docMetas) (s tutMetas) (m posts) (m docs) (m tutorials) (m pages)

instance Monoid Cache where
  mempty = Cache [] [] [] [] [] [] [] []

