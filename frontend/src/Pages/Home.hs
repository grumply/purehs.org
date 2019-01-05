module Pages.Home where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties
import Pure.Router
import Pure.Theme

import Shared.Colors
import Shared.Components.GitHubLogo
import Shared.Components.Header
import Shared.Components.Logo
import Shared.Components.Nav
import Shared.Styles

import Scope hiding (has,none,transform)

homePage :: PageScope => View
homePage =
  Div <| Theme HomeT . Theme PageT |>
    [ header
    , intro
    ]

data HomeT = HomeT
instance Themeable HomeT where
  theme c _ = void $ do
    is c .> do
      height =: per 100
      display =: flex
      flexDirection =: column
      background =: linearGradient(deg 150
                                    <&>> darkLavender <<>> per 15
                                    <&>> blueHighlight <<>> per 70
                                    <&>> lightGreen <<>> per 95
                                  )

intro =
  Div <| Theme IntroT |>
    [ hero ]

data IntroT = IntroT
instance Themeable IntroT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      flex =: one
      color =: baseWhite
      alignItems =: center

    is c $
      headerOffset

hero =
  Div <| Theme HeroT |>
    [ slogan
    , description
    , callToAction
    ]

data HeroT = HeroT
instance Themeable HeroT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      flex =: one
      flexDirection =: column
      textAlign =: center
      fontWeight =: int 200

slogan =
  H1 <| Theme SloganT |> -- [ ClassList [ "tag" ] ]
    [ Span <||> [ "The web from a " ]
    , Span <||> [ I <||> [ "different angle." ] ]
    ]

data SloganT = SloganT
instance Themeable SloganT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      flex =: one
      justifyContent =: center
      flexDirection =: column
      fontWeight =: int 200
      fontSize   =: pxs 40
      textShadow =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

    is c . has "span" .> do
      display =: flex
      flex =: one
      justifyContent =: center

description =
  P <| Theme DescriptionT |>
    [ "Pure is a Haskell-based web stack that focuses on"
    , Br
    , "performance, expressiveness, and asynchrony."
    ]

data DescriptionT = DescriptionT
instance Themeable DescriptionT where
  theme c _ = void $ do
    is c .> do
      display =: "inline-flex"
      flex =: one
      justifyContent =: center
      fontSize =: pxs 16
      textShadow =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

callToAction =
  Div <| Theme CallToActionT |>
    [ getStarted
    , startTutorial
    ]

data CallToActionT = CallToActionT
instance Themeable CallToActionT where
  theme c _ = void $ do
    is c .> do
      display =: flex
      flexDirection =: row
      justifyContent =: center

getStarted =
  A <| lref "/doc" . Theme GetStartedT . Theme CallToActionT |>
    [ "Get Started" ]

data GetStartedT = GetStartedT
instance Themeable GetStartedT where
  theme c _ = void $ do
    is c $ do
      let buttonBoxShadow opacity vOff blur vOff' blur' =
                 zero <<>> pxs vOff  <<>> pxs blur  <<>> darkLavender -- hsla(250,48.59,69,opacity)
            <&>> zero <<>> pxs vOff' <<>> pxs blur' <<>> rgba(0,0,0,0.1)

      apply $ do
        display =: inlineBlock
        backgroundColor =: baseGreen
        color =: baseWhite
        margin =: pxs 16
        height =: pxs 40
        lineHeight =: pxs 40
        padding =: zero <<>> pxs 14
        boxShadow =: buttonBoxShadow 0.13 4 6 1 3
        borderRadius =: pxs 5
        fontSize     =: pxs 15
        fontWeight   =: int 600
        textTransform =: uppercase
        "letter-spacing" =: ems 0.025
        textDecoration =: none
        transition =: "all" <<>> sec 0.1 <<>> easeInOut

      is hovered .> do
        transform =: translateY(pxs (-3))
        boxShadow =: buttonBoxShadow 0.11 10 14 3 6

      is active .> do
        transform =: translateY(pxs 5)
        boxShadow =: buttonBoxShadow 0.13 4 6 1 3

startTutorial =
  A <| lref "/tutorial" . Theme StartTutorialT . Theme CallToActionT |>
    [ "Start Tutorial" ]

data StartTutorialT = StartTutorialT
instance Themeable StartTutorialT where
  theme c _ = void $ do
    is c $ do
      let buttonBoxShadow opacity vOff blur vOff' blur' =
                 zero <<>> pxs vOff  <<>> pxs blur  <<>> darkLavender -- hsla(250,48.59,69,opacity)
            <&>> zero <<>> pxs vOff' <<>> pxs blur' <<>> rgba(0,0,0,0.1)

      apply $ do
        display =: inlineBlock
        backgroundColor =: baseWhite
        color =: baseLavender
        margin =: pxs 16
        height =: pxs 40
        lineHeight =: pxs 40
        padding =: zero <<>> pxs 14
        boxShadow =: buttonBoxShadow 0.13 4 6 1 3
        borderRadius =: pxs 5
        fontSize     =: pxs 15
        fontWeight   =: int 600
        textTransform =: uppercase
        "letter-spacing" =: ems 0.025
        textDecoration =: none
        transition =: "all" <<>> sec 0.1 <<>> easeInOut

      is hovered .> do
        transform =: translateY(pxs (-3))
        boxShadow =: buttonBoxShadow 0.11 10 14 3 6

      is active .> do
        transform =: translateY(pxs 5)
        boxShadow =: buttonBoxShadow 0.13 4 6 1 3


-- wave =
--   Svg <| Theme WaveT . ViewBox "0 0 1200 28" |>
--     [ Path <| Fill baseWhite
--             . Transform (translate(int 600 <&>> int 13) <<>> scale(negativeOne <&>> negativeOne) <<>> translate(neg (int 600) <&>> neg (int 15)))
--             . D "M0,0 L1200,0 L1200,7 C1200,7 1154,17 1024,18 C895,19 653,8 412,8 C171,8 0,21 0,21 L0,0 Z"
--     ]

-- data WaveT = WaveT
-- instance Themeable WaveT where
--   theme c _ = void $ do
--     is c .> do
--       position =: absolute
--       bottom =: zero
--       width =: per 100

