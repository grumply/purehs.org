module View.Docs.Package where

import Themes
import Types
import Utils

import Shared.Cache as Cache (Cache(docMetas,packages))
import Shared.Doc as Doc (Doc(meta),Meta(package,version))
import Shared.Package as Package (Package(meta,content),Meta(package,synopsis))

import Pure.Data.Try
import Pure.Elm
import Pure.Router (lref)

import Data.List as List

package :: Model -> Txt -> View
package model pkg
  | Just (Done p) <- lookup pkg (Cache.packages (cache model))
  , ds <- List.filter ((== pkg) . Doc.package) (Cache.docMetas (cache model))
  = listing p (fmap Doc.version ds)

  | otherwise
  = notExist

notExist = Div <||> [ "Does not exist." ]

listing :: Package.Package -> [Txt] -> View
listing p versions =
  let
    pm = Package.meta p
    ref v = "/doc/" <> Package.package pm <> "/" <> v
  in
    Article <| Theme PackageT |>
      [ Header <||>
        [ H1 <||>
          [ A <| lref (ref (List.last versions)) |>
            [ text (Package.package pm) ]
          ]
        ]
      , Section <||>
        (fmap captureLocalRefs (Package.synopsis pm))
      , Aside <||>
        [ Nav <| Theme VersionsT |>
          [ P <||> do
            List.intersperse (Span <||> [ "/" ]) $
              [ A <| lref (ref v) |>
                [ text v ]
              | v <- versions
              ]
          ]
        ]
      , Main <||>
        (fmap captureLocalRefs (Package.content p))
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

