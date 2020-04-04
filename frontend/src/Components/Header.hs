module Components.Header where

import Components.Icons
import Components.Nav
import Data.Route
import Styles.Themes

import Pure.Elm hiding (left,right)
import qualified Pure.Elm as Pure

header :: Route -> Bool -> View
header rt transp =
  let 
    t = if transp then id else Theme SolidHeaderT
    b = if transp then barMinusLogo else bar
  in 
    Header <| Theme HeaderBaseT . t |> [ b rt ]

bar_ :: Bool -> Route -> View
bar_ b rt =
  let 
    l = if b then left else Null
    r = right rt
  in
    Div <| Theme BarT |> [ l , r ]

bar :: Route -> View
bar = bar_ True

barMinusLogo :: Route -> View
barMinusLogo = bar_ False

left :: View
left = 
  Div <| Theme LeftT |>
    [ logo False True HeaderLogoT ]

right :: Route -> View
right rt =
  Div <| Theme RightT |>
    [ nav rt
    , gitHubLink "https://github.com/grumply/pure" HeaderGitHubLinkT
    ]

data SolidHeaderT = SolidHeaderT
instance Themeable SolidHeaderT where
  theme c _ = void $ is c .> do
    backgroundColor =: hsla(250,49,49,0.9)

data HeaderBaseT = HeaderBaseT
instance Themeable HeaderBaseT where
  theme c _ = void $ is c .> do
    position  =: absolute
    top       =: zero
    Pure.left =: zero
    zIndex    =: int 100
    width     =: per 100

data BarT = BarT
instance Themeable BarT where
  theme c _ = void $ is c $ do
    apply $ do
      paddingLeft   =: pxs 20
      paddingRight  =: pxs 20
      marginLeft    =: auto
      marginRight   =: auto
      height        =: pxs 60
      display       =: flex
      alignItems    =: center
      justifyContent =: spaceBetween
      flexDirection =: row

    atMedia "(max-width: 768px)" .> do
      paddingLeft =: pxs 15
      paddingRight =: pxs 15
      height =: pxs 40

    atMedia "(min-width: 780px)" .>
      width =: per 90

    atMedia "(min-width: 1340px)" .>
      maxWidth =: pxs 1260

data LeftT = LeftT
instance Themeable LeftT where
  theme c _ = void $ is c .> do
    display =: flex
    justifyContent =: flexStart

data HeaderLogoT = HeaderLogoT
instance Themeable HeaderLogoT where
  theme c _ = void $ is c $ do
    apply $
      width  =: pxs 100

    atMedia "(max-width: 48em)" .>
      width  =: pxs 85

data RightT = RightT
instance Themeable RightT where
  theme c _ = void $
    is c .> do
      display =: flex
      width =: per 100
      justifyContent =: flexEnd

data HeaderGitHubLinkT = HeaderGitHubLinkT
instance Themeable HeaderGitHubLinkT where
  theme c _ = void $
    is c $ do
      let fill = "fill"
      apply $ do
        marginLeft =: pxs 20
        fill =: white

      atMedia "(max-width: 500px)" .>
        display =: none

      has "svg" .> do
        marginLeft =: auto
        width  =: pxs 30
