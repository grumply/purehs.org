module Module (module_) where

import App (req,App)
import qualified Components.Problem as Problem
import qualified Components.Markdown as Markdown
import qualified Components.More as More
import qualified Components.Searcher as Searcher
import qualified Components.Title as Title
import Data.Entity
import Data.Route
import Styles.Themes 

import Shared
import Shared.Package as Package
import Shared.Types as Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producing)

import Control.Monad (when)
import qualified Data.List as List
import Data.Maybe (isNothing)

module_ :: App.App => PackageName -> Types.Version -> ModuleName -> View
module_ pn v mn = producing producer (consuming consumer)
  where
    producer = do
      mp <- App.req backend getPackage pn
      mv <- App.req backend getPackageVersion (pn,v)
      ms <- App.req backend listPackageVersionModulesContent (pn,v)
      when (isNothing mp) do
        retitle "Module Not Found"
      pure do
        p <- mp
        v <- mv
        pure (p,v,ms)

    consumer (Just (Package {..},Package.Version {..},ms))
      | Just m <- List.find (\(Module {..},_) -> mn == name) ms
      , es     <- entities pn v m
      = Searcher.searcher listing es
      where
        listing x search es = 
          Div <| Themed @Searcher . Themed @Hide . Themed @Load |>
            [ Input <| Value x . OnInput (withInput search) . Placeholder "Search Module"
            , Div <| Themed @Subarticles |>
              [ Div <| Themed @Article |>
                [ Header <| Themed @Header |>
                  [ Title.title (PackageRoute (PackageEntityR pn v mn en)) (toTxt en)
                  ]
                , Markdown.markdown $ Rendered (vs' ++ more)
                ]
              | Entity ety en (EntityView vs) <- es
              , let vs' = linkEntities pn v mn vs
              , let more = [ Div <| Themed @More.More |> [ A <| url Href Href (location (PackageRoute (PackageEntityR pn v mn en))) |> [ "See More >" ]] ]
              ]
            ]
    consumer _ = Problem.notFound "Module"

data Hide
instance Theme Hide where
  theme c =
    is c do
      has (subtheme @More.More) do
        display =: none

      has ".hide" do
        display =: none

        -- display any .more elements iff there is a .hide 
        -- element before it at the same level
        nexts (subtheme @More.More) do
          display =: initial

