module View.Blog where

import Pure.Data.CSS
import Pure.Data.Try
import Pure.Elm
import Pure.Theme

import Colors
import qualified Shared (Cache(..),Post(..),PostMeta(..))
import Themes
import Types
import Utils

import Components.Header (header,headerOffset)
import Components.Icons  (logo)
import Components.Titler (titler)

import Control.Monad

blog :: Model -> View
blog model =
  Div <| Theme PageT . Theme BlogT |>
    [ header (route model) False 
    , Div <| Theme ContentT |>
        [ case route model of
            BlogR Nothing  -> listing model
            BlogR (Just s) -> post s model
        ]
    , titler $
        case route model of
          BlogR (Just s) -> "Pure - " <> s
          _              -> "Pure - Blog"
    ]
          
listing model = 
  Div <| Theme ListingT |>
    ( H1 <| Theme HeaderT |>
      [ "Blog" ]
    : [ postMeta pm 
      | pm <- Shared.postMetas (cache model) 
      ]
    )

postMeta Shared.PostMeta {..} =
  let
    ref = "/blog/" <> slug
  in
    Div <| Theme ItemT . lref ref |>
      [ Div <| Theme TitleT |> [ text title ]
      , Div <| Theme DateT  |> [ text year, "-", text month, "-" , text day ]
      ]

post s model =
  Div <| Theme PostT |>
    [ case lookup s (Shared.posts (cache model)) of
        Just (Done p) -> success p
        Just Failed   -> failed
        _             -> loading
    ]

failed = 
  Div <| Theme FailedT |> 
    [ "Could not find post" ]

success p =
  Div <| Theme MarkdownT . Theme SuccessT |> 
    (fmap captureLocalRefs (Shared.content p))

loading =
  Div <| Theme LoadingT |>
    [ "Loading post" ]

data BlogT = BlogT
instance Themeable BlogT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data ListingT = ListingT
instance Themeable ListingT where
  theme c _ = void $
    is c $ do
      headerOffset
      apply $ do
        width       =: per 100
        maxWidth    =: pxs 1200
        padding     =: ems 2
        marginLeft  =: auto
        marginRight =: auto
        marginTop   =: ems 2

data ContentT = ContentT
instance Themeable ContentT where
  theme c _ = void $
    is c $ do
      headerOffset
      apply $ do
        width       =: per 100
        maxWidth    =: pxs 1200
        padding     =: ems 2
        marginLeft  =: auto
        marginRight =: auto
        marginTop   =: ems 2

data PostT = PostT
instance Themeable PostT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3


data ItemT = ItemT
instance Themeable ItemT where
  theme c _ = void $ is c $ return ()

data TitleT = TitleT
instance Themeable TitleT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 2

data DateT = DateT
instance Themeable DateT where
  theme c _ = void $ do
    is c .> do
      fontSize =: ems 1
      color =: darkGray


data HeaderT = HeaderT
instance Themeable HeaderT where
  theme c _ = void $ do
    is c $ do
      apply $ do
        fontSize =: pxs 60
        color =: darkGray
      atMedia "(max-width: 779px)" . is c .> do
        fontSize =: pxs 40

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data FailedT = FailedT
instance Themeable FailedT where
  theme c _ = void $ is c $ return ()

data SuccessT = SuccessT
instance Themeable SuccessT where
  theme c _ = void $ do
    is c $ do
      has "h2" .> do
        marginTop =: ems 2
        padding =: pxs 8
        backgroundColor =: lightLavender

