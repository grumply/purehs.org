module Package where
  
import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.Author as Author
import qualified Components.Published as Published
import qualified Components.Problem as Problem
import qualified Components.Markdown as Markdown
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import qualified Components.Version as Version
import Data.Route
import Styles.Themes as Themes

import Shared
import Shared.Package as Package
import Shared.Types

import Pure.Elm.Application
import Pure.Maybe (consuming, producingKeyed)

import GHC.Exts (IsList(..))

package :: App.App => PackageName -> View
package pn = producingKeyed pn producer (\_ -> consuming consumer)
  where
    producer pn = do
      mp  <- App.req backend getPackage pn
      mpc <- App.req backend getPackageContent pn
      vs  <- App.req backend listPackageVersions pn
      pure do
        p  <- mp
        pc <- mpc
        pure (p,pc,vs)

    consumer :: Maybe (Package,PackageContent Rendered,[Package.Version Rendered]) -> View
    consumer Nothing = Problem.notFound "Package"
    consumer (Just (p@Package {..},PackageContent md,vs)) = 
      Div <| Themed @Load |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars (author : toList collaborators)
            , Title.title (PackageRoute (PackageR name)) (toTxt name)
            , Author.author author
            , Published.published published
            , Tags.tags tags 
            , Div <| Themed @Versions |>
              [ Version.version (v == latest) name v
              | v <- fmap Package.version vs
              ]
            ]
          , Markdown.markdown md
          ]
        , Div <| Themed @Subarticles |> 
          [ H2 <||> [ "Versions" ]
          , Div <||> 
            [ Article <| Themed @Article |> 
              [ Header <||> 
                [ Title.title (PackageRoute (PackageVersionR name version)) (toTxt version)
                ]
              , Markdown.markdown cs
              ]
            | Package.Version {..} <- vs
            , let Changes cs = changes
            ]
          ]
        ]

data Versions
instance Theme Versions where
  theme c =
    is c do
      has (subtheme @Themes.Version) do
        color =: toTxt gray

        at @Latest do 
          color =: toTxt black
     
        hover do
          color =: toTxt green

          visited do
            color =: toTxt green

            at @Latest do
              color =: toTxt green

        visited do
          color =: toTxt gray

          at @Latest do 
            color =: toTxt black
