module View.Docs where

import Pure.Elm
import Pure.Data.JSON (logJSON)

import Themes
import Types
import Utils

import Components.Header (header)
import Components.Titler (titler)

import View.Docs.Packages (packages)
import View.Docs.Package (package)
import View.Docs.Version (version)
import View.Docs.Module (packageModule)
import View.Docs.Entity (entity)

data DocsMsg = Load

docs :: Model -> View
docs mdl = run (App [Load] [Load] [] () update view) mdl

update :: DocsMsg -> Model -> () -> IO ()
update _ model _ = do
  logJSON ("update" :: Txt ,model)
  case route model of
    -- * `LoadDoc p v` is idempotent, so published LoadDoc messages can be freely duplicated at little cost
    PackageR p       -> publish (LoadPackage p)
    VersionR p v     -> publish (LoadDoc p v)
    ModuleR  p v _   -> publish (LoadDoc p v)
    EntityR  p v _ _ -> publish (LoadDoc p v)
    _                -> pure ()

view :: Model -> () -> View
view model _ =
  Div <| Theme PageT . Theme ContentfulT |>
    [ header (route model) False
    , Div <| Theme ContentT |>
      [ case route model of
          PackageR p       -> package model p
          VersionR p v     -> version model p v
          ModuleR  p v m   -> packageModule model p v m
          EntityR  p v m e -> entity model p v m e
          _                -> packages model
      ]
    , titler $
        case route model of
          PackageR p       -> "Pure - " <> p
          VersionR p v     -> "Pure - " <> p <<>> v
          ModuleR  p v _   -> "Pure - " <> p <<>> v
          EntityR  p v _ _ -> "Pure - " <> p <<>> v
          _                -> "Pure Packages"
    ]
