{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Package where

import qualified Shared.Types as Types
import Shared.Types
  ( ModuleName
  , PackageName
  , Synopsis
  , Changes
  , Description
  , Name
  , Markdown
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

import Pure.Data.Txt.Search (Search)

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
  , synopsis :: Synopsis
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Search)

instance Eq (Package format) where
  (==) = (==) `on` ((name :: Package format -> PackageName) &&& latest)

instance Ord (Package format) where
  compare = compare `on` ((name :: Package format -> PackageName) &&& latest)

type PackageYaml = Package Txt
type PackageView = Package Markdown

data Version format = Version
  { version :: Types.Version
  , changes :: Changes format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Eq (Version format) where
  (==) = (==) `on` (version :: Version format -> Types.Version)

-- Ord on version is a little funky, so omitted

type VersionYaml = Version Txt
type VersionView = Version Markdown

data Module format = Module
  { name :: ModuleName
  , synopsis :: Synopsis 
  , description :: Description
  , excerpt :: Excerpt format
  } deriving (Generic,ToJSON,FromJSON,Functor,Foldable,Search)

instance Eq (Module format) where
  (==) = (==) `on` (name :: Module format -> ModuleName)

instance Ord (Module format) where
  compare = compare `on` (name :: Module format -> ModuleName)

type ModuleYaml = Module Txt
type ModuleView = Module Markdown

newtype ModuleContent content = ModuleContent content
  deriving (Functor,Foldable)
  deriving (Generic,ToTxt,ToJSON,FromTxt,FromJSON)
    via content

type ModuleContentMarkdown = ModuleContent Txt
type ModuleContentView = ModuleContent Markdown