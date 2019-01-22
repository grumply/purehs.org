module Pages.Post (postPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Data.Txt as Txt
import Pure.Router
import Pure.Theme

import Containers.Post
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

postPage :: (PostScope, PageScope) => View
postPage = withPost $ \p ->
  Div <| Theme PostPageT . Theme PageT |>
    [ header
    , Div <| Theme PostContainerT |>
      [ fetcher post
      ]
    , titler ("Pure - " <> p)
    ]

post Nothing =
  Div <| Theme NoPostT |>
    [ "Post not found." ]

post (Just Post { meta = PostMeta {..}, ..}) =
  Div <| Theme MarkdownT . Theme PostT |>
    (fmap captureLocalRefs content)

data PostPageT = PostPageT
instance Themeable PostPageT where
  theme c _ = void $ do
    is c .> do
      minHeight     =: per 100
      display       =: flex
      flexDirection =: column
      background    =: baseWhite
      paddingTop    =: ems 3
      paddingBottom =: ems 3

data PostContainerT = PostContainerT
instance Themeable PostContainerT where
  theme c _ = void $ do
    is c $ do
      headerOffset

      apply $ do
        maxWidth   =: pxs 1200
        margin     =: auto
        padding    =: ems 1

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data NoPostT = NoPostT
instance Themeable NoPostT where
  theme c _ = void $ is c $ return ()

data PostT = PostT
instance Themeable PostT where
  theme c _ = void $ do
    is c $ do
      has "h2" .> do
        marginTop =: ems 2
        padding =: pxs 8
        backgroundColor =: lightLavender
