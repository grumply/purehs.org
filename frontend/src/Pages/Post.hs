module Pages.Post (postPage) where

import Pure hiding (Transform)
import Pure.Data.CSS
import Pure.Router
import Pure.Theme

import Containers.Post
import Shared.Colors
import Shared.Components.Header
import Shared.Styles

import Scope hiding (has,none,transform)

postPage :: (PostScope, PageScope) => View
postPage =
  Div <| Theme PostPageT . Theme PageT |>
    [ header
    , Div <| Theme PostContainerT |>
      [ container loading post
      ]
    ]

loading =
  Div <| Theme LoadingT

post Nothing =
  Div <| Theme NoPostT |>
    [ "Post not found." ]

post (Just p) =
  Div <| Theme PostT |>
    (pContent p)

data PostPageT = PostPageT
instance Themeable PostPageT where
  theme c _ = void $ is c $ return ()

data PostContainerT = PostContainerT
instance Themeable PostContainerT where
  theme c _ = void $ is c $ return ()

data LoadingT = LoadingT
instance Themeable LoadingT where
  theme c _ = void $ is c $ return ()

data NoPostT = NoPostT
instance Themeable NoPostT where
  theme c _ = void $ is c $ return ()

data PostT = PostT
instance Themeable PostT where
  theme c _ = void $ is c $ return ()
