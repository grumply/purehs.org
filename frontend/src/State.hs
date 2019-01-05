module State where

import Pure (Time)
import Pure.Data.JSON (ToJSON,FromJSON)

import App
import Routes
import Shared

import GHC.Generics

data State = State
    { _stateStartup :: Time
    } deriving (Generic,ToJSON,FromJSON)
