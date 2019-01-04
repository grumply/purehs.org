{-# LANGUAGE TemplateHaskell #-}
module Lenses where

import Shared
import State

import Control.Lens.TH

-- These instances must be colocated.

makeFields ''State
