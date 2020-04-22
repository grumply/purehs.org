module Components.Breadcrumbs (breadcrumbs) where

import qualified App
import Data.Route as Route
import Styles.Colors
import Styles.Themes

import Pure.Elm.Application

import Data.List (intersperse)

breadcrumbs :: App.App => Route -> View
breadcrumbs r =
  case r of
    VersionR p v     -> bc [ pkg False p, ver True p v ]
    ModuleR  p v m   -> bc [ pkg False p, ver False p v, mdl True p v m ]
    EntityR  p v m e -> bc [ pkg False p, ver False p v, mdl False p v m, ent e ]
    _                -> Null
  where
    bc cs = 
      Nav <| Theme BreadcrumbsT . Attribute "aria-label" "Secondary Navigation" |> 
        [ P <||> (intersperse (Span <||> [ "/" ]) cs) ]

linkB True _ = id
linkB _ l = link l

pkg :: App.App => Bool -> Txt -> View
pkg b p =
  A <| linkB b (PackageR p) |>
    [ fromTxt p ]

ver :: App.App => Bool -> Txt -> Maybe Txt -> View
ver b p v =
  A <| linkB b (VersionR p v) |>
    [ maybe "latest" fromTxt v ]

mdl :: App.App => Bool -> Txt -> Maybe Txt -> Txt -> View
mdl b p v m =
  A <| linkB b (ModuleR p v m) |>
    [ fromTxt m ]

ent :: Txt -> View
ent e =
  A <||>
    [ fromTxt e ]

data BreadcrumbsT = BreadcrumbsT
instance Themeable BreadcrumbsT where
  theme c _ = void $
    is c $ do
      id .> do
        maxWidth  =: per 100
        height =: pxs 60
        overflowY =: hidden

      child "p" .> do
        height =: pxs 90
        overflowX =: scroll
        "-webkit-overflow-scrolling" =: "touch"

      has "span" .> do
        fontSize =: pxs 24
        color =: darkGray
        marginLeft =: pxs 12
        marginRight =: pxs 12

      has "a" .> do
        whiteSpace =: nowrap
        fontSize =: pxs 24
        color =: baseBlue
        textDecoration =: none

      has "a" . is ":last-child" .> do
        color =: darkGray

