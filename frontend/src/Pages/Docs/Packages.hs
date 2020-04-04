{-# language DeriveAnyClass #-}
module Pages.Docs.Packages (packages) where

import qualified App 
import Components.Searcher (searcher)
import Data.Route as Route
import Pages.Docs.Styles
import Styles.Colors
import Styles.Themes
import Styles.Responsive

import qualified Shared.Package as Package (Meta(package,synopsis))
import qualified Shared.Doc as Doc (Meta(package,version))
import Shared.Cache as Cache (Cache(packageMetas,docMetas))

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Elm.Application

import Data.Functor ((<&>))
import Data.List as List
import Data.Maybe (mapMaybe)

deriving instance Search Package.Meta

packages :: App.App => View
packages = searcher listing versions

versions :: App.App => [(Package.Meta,[Txt])]
versions = Cache.packageMetas (App.cache session) <&> \pm ->
  let pkg = Package.package pm
      match dm
        | Doc.package dm == pkg = Just (Doc.version dm)
        | otherwise = Nothing
   in (pm,mapMaybe match (Cache.docMetas (App.cache session)))

listing :: App.App => (Txt -> IO ()) -> Maybe [(Package.Meta,[Txt])] -> View
listing search mpvs =
  Div <| Theme DocT |>
    [ Div <| Theme SidebarT |>
      [ H2 <| Theme SidebarHeaderT |> [ "Top Packages" ]
      , Ul <| Theme TopPackagesT |>
        [ topPackage p
        | p <- topPackages
        ]
      ]
    , Div <||>
      [ Input <| Theme SearcherT . OnInput (withInput search) . Placeholder "Search" . AutoFocus "true"
      , results mpvs
      ]
    ]

topPackages = 
  [ "pure-elm"
  , "pure-server"
  , "pure-router"
  , "pure-websocket"
  , "pure-txt"
  , "pure-css"
  , "pure-theme"
  , "pure-svg"
  , "pure-events"
  , "pure-html"
  , "pure-json"
  , "pure-styles"
  , "pure-bench"
  , "pure-test"
  , "pure-random-pcg"
  , "excelsior"
  , "sorcerer"
  ]

topPackage :: Txt -> View
topPackage p = Li <||> [ A <| link (VersionR p Nothing) |> [ text p ] ]

results :: App.App => Maybe [(Package.Meta,[Txt])] -> View
results mpvs = 
  Div <| Theme ListingT |>
    case mpvs of
      Nothing  -> [ package p vs | (p,vs) <- versions ]
      Just pvs -> [ package p vs | (p,vs) <- pvs      ]

package :: App.App => Package.Meta -> [Txt] -> View
package pm versions =
  Article <| Theme PackageT |>
    [ Header <||>
      [ H2 <||>
        [ A <| link (VersionR (Package.package pm) (Just $ List.last versions)) |>
          [ text (Package.package pm) ]
        ]
      ]
    , Main <||>
      fmap processLinks (Package.synopsis pm)
    , Footer <||>
      [ Nav <| Theme VersionsT |>
        [ P <||> do
          List.intersperse (Span <||> [ "/" ]) $
            [ A <| link (VersionR (Package.package pm) (Just v)) |>
              [ text v ]
            | v <- versions
            ]
        , P <||>
          [ A <| link (PackageR (Package.package pm)) |>
            [ "Changelog" ]
          ]
        ]
      ]
    ]

data SidebarT = SidebarT
instance Themeable SidebarT where
  theme c _ = void $ 
    is c $ do
      apply $ display =: none 
      
      largeScreens <#> do
        display =: block
        height =: per 100
        padding =: pxs 20
        marginTop =: pxs 20
        marginRight =: pxs 20 
        marginBottom =: pxs 20 
        marginLeft =: pxs 40
        borderLeft =: pxs 1 <<>> solid <<>> "#eeeeee"

data SidebarHeaderT = SidebarHeaderT
instance Themeable SidebarHeaderT where
  theme c _ = void $ is c .> do
    marginTop =: pxs 0

data SearcherT = SearcherT
instance Themeable SearcherT where
  theme c _ = void $ 
    is c $ do
      apply $ do
        fontSize =: pxs 24
        width =: per 100
        marginTop =: pxs 24
        marginBottom =: pxs 24
        borderRadius =: pxs 8
        border =: pxs 1 <<>> solid <<>> "#eeeeee"
        outline =: none
        lineHeight =: dec 1.2
        padding =: pxs 10

      atMedia "(max-width: 940px)" .> do
        order =: neg one

data ListingT = ListingT
instance Themeable ListingT where
  theme c _ = void $ is c .> pure ()

data TopPackagesT = TopPackagesT
instance Themeable TopPackagesT where
  theme c _ = void $ is c .> do
    listStyle =: none
    paddingLeft =: pxs 8

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

