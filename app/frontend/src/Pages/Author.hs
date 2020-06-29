module Pages.Author where

import Components.Article
import Components.Author
import Components.Avatar as Avatar
import Components.Icons
import Components.Markdown
import Components.Listing
import Components.Title as Title
import Data.Placeholders
import Data.Render
import Data.Resource
import Data.Route
import Styles.Themes hiding (wait)

import Shared.Author as Author
import Shared.Types (Rendered)

import Pure.Elm.Application hiding (render,wait)
import Pure.Maybe

import Control.Concurrent.Async (wait)

import Control.Monad

newtype AuthorHeader = AuthorHeader (Author.Author Rendered)
instance Render (Route,AuthorHeader) where
  render (rt,AuthorHeader Author.Author {..}) =
    Header <| Themed @HeaderT |> 
        [ render $ Avatar.Avatars [name]
        , render $ Title.Title rt (toTxt name)
        , maybe Null render github
        , maybe Null render twitter
        , maybe Null render email
        , maybe Null render company
        ]

instance Render (Route,(Request (Maybe (Author.Author Rendered)),Request (Maybe (AuthorContent Rendered)))) where
  render (rt,(a,acv)) =
    Tagged @(Maybe (Author.Author Rendered)) $
      producing (either titled (wait >=> titled) a) 
        (consumingWith options (consumer True))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ Nothing  = notFound "Author"
      consumer b (Just v) = 
        article b (render (rt,AuthorHeader v)) (render (rt,acv)) Null

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Just placeholderAuthorView) <| Themed @PlaceholderT)

instance Render (Route,Request (Maybe (AuthorContent Rendered))) where
  render (rt,pcv) = 
    Tagged @(Maybe (AuthorContent Rendered)) $
      producing (either pure wait pcv) 
        (consumingWith options consumer)
    where
      consumer Nothing = Null
      consumer (Just (AuthorContent md)) = render md

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer (Just placeholderAuthorContentView) <| Themed @PlaceholderT)

instance Render (ListItem (Author.Author Rendered)) where
  render (ListItem rt b _ a@Author.Author {..}) = 
    article b (render (rt,AuthorHeader a)) (render excerpt) Null

instance Render (Route,Request [Author.Author Rendered]) where
  render (rt,as) = 
    Tagged @[Author.Author Rendered] $
      producing (either pure wait as) 
        (consumingWith options (consumer True id))
    where
      consumer b f = render . Listing b rt f (const Null)

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) [placeholderAuthorView])
