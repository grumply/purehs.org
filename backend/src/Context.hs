module Context where

import Pure.Capability
import Pure.Capability.TH

import Services.Docs
import Services.Examples
import Services.Posts
import Services.Tutorials

data Ctx m = Ctx
  { docs :: Docs m
  , examples :: Examples m
  , posts :: Posts m
  , tutorials :: Tutorials m
  }
mkContext ''Ctx

productionCtx = Ctx 
  productionDocs 
  productionExamples 
  productionPosts 
  productionTutorials
