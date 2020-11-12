module Components.Description (description) where

import Pure.Elm hiding (Description)

import Shared.Types

description :: Description -> View
description (Description d) =
  P <| Themed @Description |> [ txt d ]

instance Theme Description where
  theme c =
    is c do
      pure ()

