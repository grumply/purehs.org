{-# LANGUAGE DeriveAnyClass #-}
module Types where

import Pure.Data.Txt (Txt)
import Pure.Data.JSON (ToJSON(..),FromJSON)
import Pure.Data.Default (Default(def))

import Shared.Cache (Cache)
import Shared.Doc (Doc)
import Shared.Package (Package)
import Shared.Post (Post)
import Shared.Page (Page)
import Shared.Tutorial (Tutorial)

import Pure.WebSocket (WebSocket)

import GHC.Generics (Generic)

data Route
  = NoR
  | HomeR
  | AboutR
  | BlogR
  | PostR Txt
  | DocsR
  | PackageR Txt -- pkg
  | VersionR Txt Txt -- pkg,ver
  | ModuleR Txt Txt Txt -- pkg,ver,module
  | EntityR Txt Txt Txt Txt -- pkg,ver,module,entity
  | TutorialsR
  | TutorialR Txt
  deriving (Generic,ToJSON)

data Model = Model
  { route :: Route
  , cache :: Cache
  , client :: Maybe WebSocket
  }

instance ToJSON Model where
  toJSON (Model r c _) = toJSON (r,c)

model = Model NoR mempty Nothing

data Msg
  = Startup
  | Route Route
  | SetCache Cache
  | LoadDoc Txt Txt
  | SetDoc Txt Txt (Maybe Doc)
  | LoadPackage Txt
  | SetPackage Txt (Maybe Package)
  | LoadPost Txt
  | SetPost Txt (Maybe Post)
  | LoadPage Txt
  | SetPage Txt (Maybe Page)
  | LoadTutorial Txt
  | SetTutorial Txt (Maybe Tutorial)
  deriving (Generic,ToJSON)

isAboutRoute AboutR = True
isAboutRoute _ = False

isBlogRoute BlogR = True
isBlogRoute (PostR _) = True
isBlogRoute _ = False

isTutorialsRoute TutorialsR = True
isTutorialsRoute (TutorialR _) = True
isTutorialsRoute _ = False

isDocsRoute DocsR = True
isDocsRoute (PackageR _) = True
isDocsRoute (VersionR _ _) = True
isDocsRoute (ModuleR _ _ _) = True
isDocsRoute (EntityR _ _ _ _) = True
isDocsRoute _ = False