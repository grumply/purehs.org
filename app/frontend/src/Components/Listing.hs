{-# language UndecidableInstances #-}
module Components.Listing where

import Components.Searcher

import Data.Render
import Data.Route

import Styles.Responsive
import Styles.Themes

import Pure.Data.Txt.Search 
import Pure.Elm.Application hiding (render)

import Data.Typeable

data Listing a = Listing Bool Route (View -> View) ([a] -> View) [a]

data ListItem a = ListItem 
  { liRoute  :: Route 
  , liLoaded :: Bool 
  , liItem   :: a
  }

instance (Search a,Render (ListItem a),Typeable a) => Render (Listing a) where
  render (Listing _ _ _ _ []) =
    Div
  render (Listing b rt f d as) =
    flip searcher as $ \_ search xs ->
      Div <| Themed @ListingT |>
        [ Input <| OnInput (withInput search) . Placeholder "Search" . Attribute "title" "Search"
        , Div <||> [ render (ListItem rt b x) <| f | x <- xs ]
        ]

data ListingT
instance Theme ListingT where
  theme c = void $ 
    is c $ do
      apply $ do
        margin-top =: (-60)px

      smallScreens <%> do
        margin-top =: 0

      has (tag Input) $ do
        apply $ do
          display       =: block
          margin        =* [30px,auto]
          font-size     =: 24px
          margin-top    =: 24px
          margin-bottom =: 24px
          border-radius =: 8px
          border        =* [1px,solid,hex 0xeee]
          outline       =: none
          line-height   =: 1.2
          padding       =: 10px
          width         =: (100%)
          margin-top    =: (-10)px
          margin-bottom =: 60px

        mediumScreens <%> do
          width =: 700px

        largeScreens <%> do
          width =: 800px

        hugeScreens <%> do
          width =: 900px

