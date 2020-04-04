module Pages.Docs.Styles where

import Styles.Colors
import Styles.Themes
import Styles.Responsive

import Pure.Elm

data DocT = DocT
instance Themeable DocT where
  theme c _ = void $ 
    is c $ do
      apply $ do
        minHeight     =: per 100
        display       =: flex
        background    =: baseWhite
        flexDirection =: column

      largeScreens <#> do
        flexDirection =: rowReverse

      child "div" $ do

        is ":first-child" $ do
          apply $ width =: per 100

          largeScreens <#> do
            padding =: pxs 20
            margin =: pxs 20 <<>> pxs 20 <<>> pxs 20 <<>> pxs 40
            borderLeft =: pxs 1 <<>> solid <<>> lightGray
            width =: pxs 260

        is ":last-child" $ do
          apply $ width =: per 100

          largeScreens <#> do
            width =: pxs 768

      has ".hide" $ do
        apply $ display =: none
        next "*" .>
          display =: none


