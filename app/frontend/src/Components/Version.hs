module Components.Version where

import Pure.Elm

import Components.Preload ( prelink )
import Data.Route

import Styles.Themes ( Latest )

import Shared.Types as Types ( Version, PackageName )

version :: Bool -> PackageName -> Types.Version -> View
version isLatest package version =
  let t | isLatest = Themed @Latest | otherwise = id
  in A <| Themed @Types.Version . t . prelink (PackageRoute (PackageVersionR package version)) |> 
      [ txt version ]

instance Theme Types.Version