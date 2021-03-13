module Entity (entity) where

import App (req,App)
import qualified Components.Problem as Problem
import qualified Components.Markdown as Markdown
import qualified Components.Title as Title
import qualified Components.More as More
import Data.Entity
import Data.Route
import Styles.Themes

import Shared
import Shared.Package as Package
import Shared.Types as Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producingKeyed)

import Control.Monad (when)
import Data.Maybe (isNothing)
import qualified Data.List as List

entity :: App.App => PackageName -> Types.Version -> ModuleName -> Txt -> View
entity pn v mn e = producingKeyed (pn,v,mn,e) producer (\(pn,v,mn,e) -> consuming (consumer (pn,v,mn,e)))
  where
    producer (pn,v,mn,e) = do
      mp <- App.req backend getPackage pn
      mv <- App.req backend getPackageVersion (pn,v)
      ms <- App.req backend listPackageVersionModulesContent (pn,v)
      when (isNothing mp) do
        retitle "Entity Not Found"
      pure do
        p <- mp
        v <- mv
        pure (p,v,ms)

    consumer (pn,v,mn,e) Nothing = Problem.notFound "Entity"
    consumer (pn,v,mn,e) (Just (Package {..},Package.Version {..},ms))
      | Just m <- List.find (\(Module {..},_) -> mn == name) ms
      , es     <- entities pn v m
      , Just e <- List.find (\(Entity _ en _) -> e == en) es
      , Entity ety en (EntityView vs) <- rebaseEntityLinks pn v mn e
      = Div <| Themed @Subarticles . Themed @More.Unhide . Themed @Load |>
        [ Div <| Themed @Article |>
          [ Header <| Themed @Header |>
            [ Title.title (PackageRoute (PackageEntityR pn v mn en)) (toTxt en)
            ]
          , Markdown.markdown $ Rendered vs
          ]
        ]