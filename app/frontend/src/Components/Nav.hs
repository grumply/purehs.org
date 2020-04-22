module Components.Nav where

import Data.Route
import Styles.Colors
import Styles.Themes

import Pure.Elm
import Pure.Router (lref)

nav :: Route -> View
nav rt =
  Nav <| Attribute "aria-label" "Primary Navigation" . Theme NavT |>
    [ navLink (check rt) l t
    | (check,l,t) <-
      [ (isAboutRoute,"/about","About")
      , (isBlogRoute,"/blog","Blog")
      , (isDocsRoute,"/doc","Docs")
      , (isTutorialsRoute,"/tut","Tutorials")
      ]
    ]
  where

    isAboutRoute = \case
      AboutR -> True
      _      -> False

    isBlogRoute = \case
      BlogR    -> True
      PostR {} -> True
      _        -> False

    isTutorialsRoute = \case
      TutorialsR   -> True
      TutorialR {} -> True
      _            -> False

    isDocsRoute = \case
      DocsR       -> True
      PackageR {} -> True
      VersionR {} -> True
      ModuleR  {} -> True
      EntityR  {} -> True
      _           -> False

navLink :: Bool -> Txt -> View -> View
navLink actv link v =
  A <| lref link . Theme LinkT |>
    [ v
    , if actv then Theme LightbarT Span else Null
    ]

data NavT = NavT
instance Themeable NavT where
  theme c _ = void $ is c .> do
    display        =: flex
    flexDirection  =: row
    overflowX      =: auto
    justifyContent =: flexEnd

data LinkT = LinkT
instance Themeable LinkT where
  theme c _ = void $ is c $ do
    apply $ do
      display         =: flex
      flexDirection   =: row
      alignItems      =: center
      alignContent    =: spaceBetween
      height          =: per 100
      paddingLeft     =: pxs 10
      position        =: relative
      fontSize        =: pxs 24
      fontWeight      =: int 200
      color           =: white
      textDecoration  =: none

    atMedia "(max-width: 48em)" .> do
      fontSize        =: pxs 18

    is ":hover" .> do
      textShadow      =: zero <<>> zero <<>> pxs 5 <<>> lightGreen

data LightbarT = LightbarT
instance Themeable LightbarT where
  theme c _ = void $ is c .> do
    position        =: absolute
    height          =: pxs 1
    left            =: pxs 10
    right           =: zero
    bottom          =: pxs 0
    backgroundColor =: blueHighlight
