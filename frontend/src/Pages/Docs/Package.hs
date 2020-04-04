module Pages.Docs.Package (package) where

import qualified App
import Data.Route as Route
import Styles.Colors
import Styles.Themes

import qualified Shared.Cache as Cache (Cache(docMetas,packages))
import qualified Shared.Doc as Doc (Meta(package,version))
import qualified Shared.Package as Pkg (Package(meta,content),Meta(package,synopsis))

import Pure.Data.Try
import Pure.Elm.Application

import Data.List as List

package :: App.App => Txt -> View
package pkg
  | Just (Done p) <- lookup pkg (Cache.packages (App.cache session))
  , ds <- List.filter ((== pkg) . Doc.package) (Cache.docMetas (App.cache session))
  = listing p (fmap Doc.version ds)

  | otherwise
  = notExist

notExist :: View
notExist = Div <||> [ "Does not exist." ]

listing :: App.App => Pkg.Package -> [Txt] -> View
listing p@(Pkg.meta -> pm) versions =
  Article <| Theme PackageT |>
    [ Header <||>
      [ H1 <||>
        [ A <| link (VersionR (Pkg.package pm) (Just $ List.last versions)) |>
          [ text (Pkg.package pm) ]
        ]
      ]
    , Section <||>
      (fmap processLinks (Pkg.synopsis pm))
    , Aside <||>
      [ Nav <| Theme VersionsT |>
        [ P <||> do
          List.intersperse (Span <||> [ "/" ]) $
            [ A <| link (VersionR (Pkg.package pm) (Just v)) |>
              [ text v ]
            | v <- versions
            ]
        ]
      ]
    , Main <||>
      (fmap processLinks (Pkg.content p))
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

