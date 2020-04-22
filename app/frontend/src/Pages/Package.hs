module Pages.Package (page) where

import qualified App
import Components.Header
import Control.Futures
import Data.Route as Route
import Styles.Colors
import Styles.Themes

import qualified Shared.Cache as Cache (Cache(docMetas))
import qualified Shared.Doc as Doc (Meta(package,version))
import qualified Shared.Package as Pkg (Package(meta,content),Meta(package,synopsis))

import Pure.Elm.Application hiding (page,Frame)
import Pure.Spinners (ChasingDots(..))

import Data.Function ((&))
import Data.List as List

page :: App.App => Txt -> View
page pkg = 
  Page $
    WithHeader (header (PackageR pkg) False) $
      package pkg

data Package

package :: App.App => Txt -> View
package pkg =
  producingKeyed @Package pkg producer 
    (consumingWith options . consumer)
  where
    producer = App.loadPackage

    consumer _ = success

    options = defaultOptions 
            & suspense (Milliseconds 500 0) loading 
            & trouble  (Seconds 5 0) problems

loading :: View
loading = WithoutSidebar (Div <||> [ View (def @ChasingDots) ])

problems :: View
problems = WithoutSidebar "Problem loading package."

failed :: View
failed = WithoutSidebar "Package not found."

success :: App.App => Maybe Pkg.Package -> View
success Nothing = failed
success (Just p)
  | pkg <- Pkg.package (Pkg.meta p)
  , ds <- List.filter ((== pkg) . Doc.package) (Cache.docMetas (App.cache session))
  = listing p (fmap Doc.version ds)

listing :: App.App => Pkg.Package -> [Txt] -> View
listing p@(Pkg.meta -> pm) versions =
  WithoutSidebar $
    Div <||>
      [ H1 <||>
        [ A <| link (VersionR (Pkg.package pm) (Just $ List.last versions)) |>
          [ text (Pkg.package pm) ]
        ]
      , Div <||>
        [ processLinks s
        | s <- Pkg.synopsis pm
        ]
      , Nav <| Theme VersionsT |>
        [ P <||> do
          List.intersperse (Span <||> [ "/" ]) $
            [ A <| link (VersionR (Pkg.package pm) (Just v)) |>
              [ text v ]
            | v <- versions
            ]
        ]
      , Section <||>
        [ processLinks c
        | c <- Pkg.content p
        ]
      ]

data PackageT = PackageT
instance Themeable PackageT where
  theme c _ = void $
    is c $ child "header" . child "h1" $ do
      id .> do
        marginBottom =: zero
      child "a" $ do
        id .> do
          fontSize =: pxs 32
          color =: darkGray
          textDecoration =: none
        is ":hover" .> do
          textDecoration =: underline
        is ":visisted" .> do
          color =: darkGray

data VersionsT = VersionsT
instance Themeable VersionsT where
  theme c _ = void $
    is c $ do
      id .> do
        display =: inlineBlock
        marginLeft =: pxs 24

      child "p" $ do
        id .> do
          marginTop =: zero

        child "span" .> do
          marginLeft =: pxs 8
          marginRight =: pxs 8
          color =: darkGray
          fontSize =: pxs 18

        child "a" .> do
          fontSize =: pxs 18
          color =: darkBlue
          textDecoration =: none

        child "a" . is ":hover" .> do
          color =: baseGreen
          textDecoration =: underline

        child "a" . is ":visited" .> do
          color =: darkGreen

        child "a" . is ":hover" . is ":visisted" .> do
          color =: darkGreen
