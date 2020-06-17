module Components.Article where

import Components.Avatar

import Styles.Colors
import Styles.Fonts
import Styles.Responsive
import Styles.Themes

import Pure.Elm.Application hiding (lavender,brightness)

import Prelude hiding (max,repeat)

article :: Bool -> View -> View -> View -> View
article loaded header content footer = 
  Article <| (if loaded then Themed @LoadT else id) . Themed @ArticleT |> 
    [ header , content , footer ]

data LoadT
instance Theme LoadT where
  theme c = void $ do
    atKeyframes "focus" $ do
      has from .> do
        opacity =: 0
        filter_ =: blur(20px)
      has to .> do
        filter_ =: none
        opacity =: 1

    is c .>
      animation =* ["focus",300ms]

data ArticleT
instance Theme ArticleT where
  theme c = void $
    is c $ do
      has ".drop" . is firstOfType . is firstLetter .> do
        font-family  =: serifFont
        font-size    =: 4em
        font-weight  =: 400
        margin-top   =: 0.2em
        margin-right =: 0.15em
        float        =: left
        color        =: toTxt lavender

      next (subtheme @SubarticlesT) .> do
        margin-top =: (-70)px

      apply $ do
        width       =: (100%)
        padding     =* [60px,30px]
        padding-top =: 110px
        margin      =* [0,auto,100px]
        background  =: hex 0xfff
        border      =* [1px,solid,hex 0xeaecee]
        position    =: relative
        width       =: (100%)
        max-width   =: (100%)

      mediumScreens <%> do
        width =: 700px

      largeScreens <%> do
        width =: 800px

      hugeScreens <%> do
        width =: 900px

data SubarticlesT
instance Theme SubarticlesT where
  theme c = void $ 
    is c $ do
      apply $ do
        padding-bottom =: 30px

      child (tag H2) $ do
        apply $ do
          font-family =: titleFont
          font-size   =: 2.5em
          font-weight =: 400
          color       =: toTxt base { brightness = 45 }
          text-align  =: center

      has (tag Article) .> do
        padding-top    =: 60px
        margin-bottom =: 50px

      has (subtheme @ArticleT) $ do
        
        apply $ do
          padding =* [30px,30px]

        child (tag Header) . is firstChild .> do
          margin-top  =: (-40)px
          text-align  =: left
          margin-left =: 20px

      has (subtheme @AvatarsT) .> do
        display =: none

data TutorialsT
instance Theme TutorialsT 

data PostsT
instance Theme PostsT
