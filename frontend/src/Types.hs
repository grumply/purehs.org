module Types where

import Pure.Data.Txt (Txt)
import Pure.Data.Default

import Shared

import Pure.WebSocket (WebSocket)

data Route
  = NoR
  | HomeR
  | AboutR
  | BlogR (Maybe Txt)
  | DocsR (Maybe (Txt,Txt))
  | TutsR (Maybe Txt)

data Model = Model
  { route :: Route
  , cache :: Cache
  , client :: Maybe WebSocket
  }

model = Model NoR mempty Nothing

data Msg
  = Startup
  | Route Route
  | SetCache Cache
  | LoadDoc Txt Txt
  | SetDoc Txt Txt (Maybe Doc)
  | LoadPost Txt
  | SetPost Txt (Maybe Post)
  | LoadPage Txt
  | SetPage Txt (Maybe Page)
  | LoadTutorial Txt
  | SetTutorial Txt (Maybe Tutorial)

isAboutRoute AboutR = True
isAboutRoute _ = False

isBlogRoute (BlogR _) = True
isBlogRoute _ = False

isTutorialsRoute (TutsR _) = True
isTutorialsRoute _ = False

isDocsRoute (DocsR _) = True
isDocsRoute _ = False
