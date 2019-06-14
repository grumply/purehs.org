module Types where

import Pure.Elm hiding (Doc)
import Pure.Data.Try

import Shared

import Data.Map

data Route
  = HomeR
  | AboutR
  | BlogR (Maybe Txt)
  | DocsR (Maybe (Txt,Txt))
  | TutsR (Maybe Txt)

data Model = Model 
  { route :: Route
  , cache :: Cache
  }

model = Model HomeR def

data Msg
  = Route Route
  | SetCache Cache
  | LoadDoc Txt Txt
  | SetDoc Txt Txt (Maybe Doc)
  | LoadPost Txt
  | SetPost Txt (Maybe Post)
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
