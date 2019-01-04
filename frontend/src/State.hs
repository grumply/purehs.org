module State where

import Pure (Time)
import Pure.Data.JSON (ToJSON,FromJSON)

import App
import Shared

import GHC.Generics

data ActivePage = BlogActive | DocsActive | TutsActive
  deriving (Eq,Enum,Generic,ToJSON,FromJSON)

data State = State
    { _stateStartup    :: Time
    , _stateActivePage :: Maybe ActivePage
    } deriving (Generic,ToJSON,FromJSON)
