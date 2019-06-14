module Context where

import Pure.Capability
import Pure.Capability.TH

import Services.Docs
import Services.Pages
import Services.Posts
import Services.Tutorials

data Ctx m = Ctx
  { docs :: Docs m
  , pages :: Pages m
  , posts :: Posts m
  , tutorials :: Tutorials m
  }
mkContext ''Ctx

productionCtx = Ctx 
  productionDocs 
  productionPages
  productionPosts 
  productionTutorials
