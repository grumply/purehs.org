module Version where

import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.Author as Author
import qualified Components.Description as Description
import qualified Components.Preload as Preload
import qualified Components.Published as Published
import qualified Components.Problem as Problem
import qualified Components.Markdown as Markdown
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import Data.Route
import Styles.Themes as Themes

import Shared
import Shared.Package as Package
import Shared.Types as Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producing)

import Control.Monad (when)
import Data.Maybe (isNothing)
import GHC.Exts (IsList(..))

version :: App.App => PackageName -> Types.Version -> View
version pn v = producing producer (consuming consumer)
  where
    producer = do
      mp <- App.req backend getPackage pn
      mv <- App.req backend getPackageVersion (pn,v)
      ms <- App.req backend listPackageVersionModulesContent (pn,v)
      when (isNothing mp) do
        retitle "Package Version Not Found"
      pure do
        p <- mp
        v <- mv
        pure (p,v,ms)

    consumer Nothing = Problem.notFound "Package Version"
    consumer (Just (Package {..},Package.Version {..},ms)) = 
      Div <| Themed @Load |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars (author : toList collaborators)
            , Title.title (PackageRoute (PackageR name)) (toTxt name)
            , Author.author author
            , Published.published published
            , Tags.tags tags  
            , let t | v == latest = Themed @Latest | otherwise = id
              in A <| Themed @Themes.Version . t . Preload.prelink (PackageRoute (PackageVersionR pn v)) |> [ txt v ]
            ]
          , Description.description description
          ]
        , Div <| Themed @Subarticles |> 
          [ H2 <||> [ "Modules" ]
          , Div <||> 
            [ Article <| Themed @Article |> 
              [ Header <||> 
                [ Title.title (PackageRoute (PackageModuleR pn v name)) (toTxt name) ]
              , Markdown.markdown md
              ]
            | Module {..} <- fmap fst ms
            , let Excerpt md = excerpt
            ]
          ]
        ]


