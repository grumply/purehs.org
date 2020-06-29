{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Package where

import qualified Shared.Types as Types
import Shared.Types
  ( ModuleName
  , PackageName
  , Short
  , Changes
  , Description
  , Name
  , Published
  , Tags
  , License
  , Repository
  , Homepage
  , Collaborators
  , Excerpt
  )

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Txt (Txt,ToTxt,FromTxt)

import Control.Arrow ((&&&))
import Data.Function (on)
import GHC.Generics (Generic)

import Pure.Data.Txt.Search (Search(..))

data Package format = Package
  { name :: PackageName
  , author :: Name
  , latest :: Types.Version
  , published :: Published
  , license :: License
  , repository :: Maybe Repository
  , homepage :: Maybe Homepage
  , collaborators :: Collaborators
  , tags :: Tags
  , short :: Short
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor)

instance Search (Package format) where
  contains so t Package {..} =
    contains so t (name,author,license,collaborators,tags,description)

instance Eq (Package format) where
  (==) = (==) `on` ((name :: Package format -> PackageName) &&& latest)

instance Ord (Package format) where
  compare = compare `on` ((name :: Package format -> PackageName) &&& latest)

newtype PackageContent content = PackageContent content
  deriving (Functor,Foldable)
  deriving (Generic,ToTxt,ToJSON,FromTxt,FromJSON)
    via content

data Version format = Version
  { version :: Types.Version
  , changes :: Changes format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Eq (Version format) where
  (==) = (==) `on` (version :: Version format -> Types.Version)

-- Ord on version is a little funky, so omitted

data Module format = Module
  { name :: ModuleName
  , description :: Description 
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Eq (Module format) where
  (==) = (==) `on` (name :: Module format -> ModuleName)

instance Ord (Module format) where
  compare = compare `on` (name :: Module format -> ModuleName)

newtype ModuleContent content = ModuleContent content
  deriving (Functor,Foldable)
  deriving (Generic,ToTxt,ToJSON,FromTxt,FromJSON)
    via content