module View.Docs.Breadcrumbs (breadcrumbs) where

import Themes
import Types

import Pure.Elm
import Pure.Router (lref)

import Data.List (intersperse)

breadcrumbs :: Route -> View
breadcrumbs r =
  case r of
    VersionR p v     -> bc [ pkg False p, ver True p v ]
    ModuleR  p v m   -> bc [ pkg False p, ver False p v, mdl True p v m ]
    EntityR  p v m e -> bc [ pkg False p, ver False p v, mdl False p v m, ent e ]
    _                -> Null
  where
    bc cs = Nav <| Theme BreadcrumbsT |> [ P <||> (intersperse (Span <||> [ "/" ]) cs) ]

link True _ = id
link _ l = lref l

pkg :: Bool -> Txt -> View
pkg b p =
  A <| link b ("/doc/" <> p) |>
    [ fromTxt p ]

ver :: Bool -> Txt -> Txt -> View
ver b p v =
  A <| link b ("/doc/" <> p <> "/" <> v) |>
    [ fromTxt v ]

mdl :: Bool -> Txt -> Txt -> Txt -> View
mdl b p v m =
  A <| link b ("/doc/" <> p <> "/" <> v <> "/" <> m) |>
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

