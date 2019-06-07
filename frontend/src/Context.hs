module Context where

import Imports

import Services.Client
import Services.Route
import Services.Storage

data Ctx m = Ctx
  { client  :: Client m
  , route   :: Route m
  , storage :: Storage m
  }
mkContext ''Ctx

productionCtx :: forall m. MonadIO m => Ctx m
productionCtx = Ctx
  productionClient
  productionRoute
  productionStorage
