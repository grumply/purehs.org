module View.Docs.Packages where

import Themes
import Types
import Utils

import qualified Shared.Package as Package (Meta(package,synopsis))
import qualified Shared.Doc as Doc (Meta(package,version))
import Shared.Cache as Cache (Cache(packageMetas,docMetas))

import Pure.Data.Txt as Txt
import Pure.Router (lref)
import Pure.Elm

import Data.Functor ((<&>))
import Data.Function (on)
import Data.List as List
import Data.Maybe (mapMaybe)

packages :: Model -> View
packages model =
  Div <||>
    [ H1 <| Theme HeaderT |>
      [ "Packages" ]
    , listing pkgs
    ]
  where
    pkgs = (Cache.packageMetas (cache model)) <&> \pm ->
      let pkg = Package.package pm
          match dm
            | Doc.package dm == pkg = Just (Doc.version dm)
            | otherwise = Nothing
       in (pm,mapMaybe match (Cache.docMetas (cache model)))

listing :: [(Package.Meta,[Txt])] -> View
listing pvs =
  Div <| Theme ListingT |>
    [ package p vs
    | (p,vs) <- pvs
    ]

package :: Package.Meta -> [Txt] -> View
package pm versions =
  let
    ref v = "/doc/" <> Package.package pm <> "/" <> v
  in
    Article <| Theme PackageT |>
      [ Header <||>
        [ H2 <||>
          [ A <| lref (ref (List.last versions)) |>
            [ text (Package.package pm) ]
          ]
        ]
      , Main <||>
        (fmap captureLocalRefs (Package.synopsis pm))
      , Footer <||>
        [ Nav <| Theme VersionsT |>
          [ P <||> do
            List.intersperse (Span <||> [ "/" ]) $
              [ A <| lref (ref v) |>
                [ text v ]
              | v <- versions
              ]
          , P <||>
            [ A <| lref ("/doc/" <> Package.package pm) |>
              [ "Changelog" ]
            ]
          ]
        ]
      ]

data ListingT = ListingT
instance Themeable ListingT where
  theme c _ = void $ is c .> pure ()

data PackageT = PackageT
instance Themeable PackageT where
  theme c _ = void $
    is c $ child "header" . child "h2" $ do
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

