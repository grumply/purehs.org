module Components.Problem where

import Pure.Elm hiding (black,lavender,green)
import Styles.Colors (black, faded, green, lavender)
import Styles.Fonts (defaultFont, titleFont)
import Styles.Responsive ((<%>), largeScreens, mediumScreens)

problems :: Txt -> View -> View
problems nm c =
    Div <||>
      [ Div <| Themed @Error |>
        [ Header <||>
          [ H1 <||> [ fromTxt (nm <> " not found.") ]
          , H2 <||> [ "We couldn't find what you were looking for." ]
          ]
        , c
        ]
      ]

notFound :: Txt -> View
notFound nm = problems nm $ Div <||>
  [ P <||> [ "Please contact the owner of the site that linked you to the original URL and let them know their link is broken." ]
  , P <||> 
    [ "If an internal link brought you here, please file a bug report "
    , A <| Rel "noopener" . Attribute "target" "_blank" . Href "https://github.com/grumply/purehs.org/issues/new" |> [ "here" ]
    , "."
    ]
  ]

emptyList :: View -> View -> View
emptyList h1 h2 = 
  Div <||>
    [ Div <| Themed @Error |>
      [ Header <||> 
        [ H1 <||> [ h1 ]
        , H2 <||> [ h2 ]
        ]
      ]
    ] 

data Error
instance Theme Error where
  theme c =
    is c do
      width  =: (90%)
      margin =* [40px,auto]

      mediumScreens <%> do
        width =: 720px

      largeScreens <%> do
        width =: 900px

      child (tag Header) do
        child (tag H1) do
          margin-top    =: 0
          margin-bottom =: 24px
          color         =: toTxt lavender
          font-size     =: 4em
          font-family   =: titleFont
        
        child (tag H2) do
          margin-bottom =: 44px
          color         =: toTxt black
          font-size     =: 1.5em
          font-family   =: titleFont

      has (tag P) do
        color =: toTxt black
        font-family =: defaultFont
        line-height =: 28px
        font-size   =: 16px

        has (tag A) do

          background-color =: toTxt (faded green)
          border-bottom    =* [1px,solid,toTxt green]
          color            =: toTxt black
          text-decoration  =: none

          hover do
            background    =: toTxt green
            border-bottom =* [1px,solid,toTxt black]

   