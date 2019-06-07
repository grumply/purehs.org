module Components.Header where

import qualified Pure (left,right)
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties

import Colors
import Context
import Imports hiding (left,right)

import Services.Client hiding (client)
import Services.Route
import Services.Storage

import Components.GitHubLogo (viewGitHubLink)
import Components.Logo (viewLogo)
import Components.Nav (viewNav)

data Env = Env Bool

data State = State

newtype HeaderM a = HeaderM { runHeaderM :: Aspect (Ctx HeaderM) Env State a }
mkAspect ''HeaderM

viewHeader :: Ctx HeaderM -> View
viewHeader c = viewHeaderM header c (Env False) State

viewHeaderTransparent :: Ctx HeaderM -> View
viewHeaderTransparent c = viewHeaderM header c (Env True) State

header :: HeaderM View
header = do
  Env transparent <- ask
  let t = if transparent then Theme HeaderTransparentT else Theme HeaderT
  b <- if transparent then barMinusLogo else bar
  pure $
    Header <| t |> 
      [ b
      ]

data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ do
    is c .> do
      position        =: absolute
      top             =: zero
      Pure.left       =: zero
      zIndex          =: int 100
      width           =: per 100
      backgroundColor =: hsla(250,49,49,0.9)

data HeaderTransparentT = HeaderTransparentT
instance Themeable HeaderTransparentT where
  theme c _ = void $ do
    is c .> do
      position        =: absolute
      top             =: zero
      Pure.left       =: zero
      zIndex          =: int 100
      width           =: per 100

bar_ :: Bool -> HeaderM View
bar_ b = do
  l <- if b then left else pure Null
  r <- right
  pure $
    Div <| Theme BarT |>
      [ l , r ]

bar :: HeaderM View
bar = bar_ True

barMinusLogo :: HeaderM View
barMinusLogo = bar_ False

left :: HeaderM View
left = do
  pure $
    Div <| Theme LeftT |>
      [ viewLogo False True HeaderLogoT ]


right :: HeaderM View
right = do
  c <- ctx >>= rebase
  pure $
    Div <| Theme RightT |>
      [ viewNav (ffmap liftIO c)
      , viewGitHubLink "https://github.com/grumply/pure" HeaderGitHubLinkT
      ]

data BarT = BarT
instance Themeable BarT where
  theme c _ = void $ do
    is c $ do
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

      atMedia "(max-width: 48em)" .> do
        paddingLeft =: pxs 15
        paddingRight =: pxs 15
        height =: pxs 40

      atMedia "(min-width: 780px)" .> do
        width =: per 90

      atMedia "(min-width: 1340px)" .> do
        maxWidth =: pxs 1260

headerOffset = do
    apply $ do
      marginTop =: pxs 75

    atMedia "(max-width: 48em)" .> do
      marginTop =: pxs 50

data LeftT = LeftT
instance Themeable LeftT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      justifyContent =: flexStart

data HeaderLogoT = HeaderLogoT
instance Themeable HeaderLogoT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        width  =: pxs 100

      atMedia "(max-width: 48em)" .> do
        width  =: pxs 85

data RightT = RightT
instance Themeable RightT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      width =: per 100
      justifyContent =: flexEnd

data HeaderGitHubLinkT = HeaderGitHubLinkT
instance Themeable HeaderGitHubLinkT where
  theme c _ = void $
    is c $ do
      apply $ do
        marginLeft =: pxs 20
        fill =: white

      atMedia "(max-width: 500px)" .> do
        display =: none

      has "svg" .> do
        marginLeft =: auto
        width  =: pxs 30
