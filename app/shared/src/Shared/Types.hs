{-# language DerivingVia, OverloadedStrings, UndecidableInstances #-}
module Shared.Types where

import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Data.Render ()
import Pure.Data.Time (Time,IsTime,toDateTime,fromDate,fromDateTime,parseTime,formatTime)
import Pure.Data.Txt as Txt (Txt,ToTxt,FromTxt,splitOn,toTxt,fromTxt)
import Pure.Data.View (View)
import Data.Time.Format (FormatTime,ParseTime)

import Control.Applicative
import Control.Monad
import Data.Function (on)
import Data.String (IsString)
import Data.List
import Data.Maybe (catMaybes)
import GHC.Exts (IsList)
import Text.Read (readMaybe)

import Pure.Data.Txt.Search

instance Search Time where
  contains so n t =
    contains so n (formatTime "%Y %m %B %m %A %e" t :: Txt)

newtype Name = Name Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype PackageName = PackageName Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype ModuleName = ModuleName Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype GitHubName = GitHubName Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype TwitterHandle = TwitterHandle Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype Email = Email Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype Company = Company Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search) 
    via Txt

newtype Title = Title Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search)
    via Txt

newtype Subtitle = Subtitle Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search)
    via Txt

newtype Slug = Slug Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search)
    via Txt

newtype Description = Description Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search)
    via Txt

newtype Series = Series Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search)
    via Txt

newtype Episode = Episode Int
  deriving (ToJSON,FromJSON,Eq,Ord,Search)
    via Int

newtype Edited = Edited Time
  deriving (ParseTime,FormatTime,IsTime,Eq,Ord,Search)
    via Time

instance ToJSON Edited where
  toJSON (Edited p) = toJSON (toDateTime p :: Txt)

instance FromJSON Edited where
  parseJSON t = do 
    txt :: Txt <- parseJSON t
    case parseTime "%Y-%m-%dT%H:%M:%S%Q" txt <|> parseTime "%Y-%m-%dT%H:%M:%S%03Q" txt <|> parseTime "%Y-%m-%d" txt of
      Just p  -> pure (Edited p)
      Nothing -> mzero

newtype Published = Published Time
  deriving (ParseTime,FormatTime,IsTime,Eq,Ord,Search)
    via Time

instance ToJSON Published where
  toJSON (Published p) = toJSON (toDateTime p :: Txt)

instance FromJSON Published where
  parseJSON t = do 
    txt :: Txt <- parseJSON t
    case parseTime "%Y-%m-%dT%H:%M:%S%Q" txt <|> parseTime "%Y-%m-%dT%H:%M:%S%03Q" txt <|> parseTime "%Y-%m-%d" txt of
      Just p  -> pure (Published p)
      Nothing -> mzero

newtype Authors = Authors [Name]
  deriving (ToJSON,FromJSON,Eq,Search,IsList)
    via [Name]
  
newtype Editors = Editors [Name]
  deriving (ToJSON,FromJSON,Eq,Search,IsList)
    via [Name]

newtype Tag = Tag Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,Ord,IsString,Search)
    via Txt

newtype Tags = Tags [Tag]
  deriving (ToJSON,FromJSON,Eq,Search,IsList)
    via [Tag]

newtype Excerpt excerpt = Excerpt excerpt
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON,IsString,Search,IsList)
    via excerpt

newtype Packages = Packages [PackageName]
  deriving (ToJSON,FromJSON,Eq,Search,IsList)
    via [PackageName]

newtype Version = Version Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,IsString,Search)
    via Txt

newtype License = License Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,IsString,Search)
    via Txt

newtype Repository = Repository Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,IsString,Search)
    via Txt

newtype Homepage = Homepage Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,IsString,Search)
    via Txt

newtype Collaborators = Collaborators [Name]
  deriving (ToJSON,FromJSON,Eq,Search,IsList)
    via [Name]

newtype Unrendered = Unrendered Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,IsString,Search)
    via Txt

newtype Rendered = Rendered [View]
  deriving (ToJSON,FromJSON,Search,IsList)
    via [View]

instance Ord Version where
  compare = compare `on` versionList
    where
      versionList :: Version -> [Int]
      versionList = 
        catMaybes . fmap (readMaybe . fromTxt) . Txt.splitOn "." . toTxt

newtype Synopsis = Synopsis Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,Eq,IsString,Search)
    via Txt
  
newtype Changes changes = Changes changes
  deriving (Functor,Foldable)
  deriving (ToTxt,ToJSON,FromTxt,FromJSON,IsString,Search,IsList)
    via changes