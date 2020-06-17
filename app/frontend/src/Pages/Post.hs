module Pages.Post where

import Components.Article
import Components.Avatar as Avatar
import Components.Author as Author
import Components.Markdown
import Components.Listing
import Components.Tags 
import Components.Title as Title
import Components.Time
import Components.Subtitle as Subtitle

import Data.Placeholders
import Data.Render
import Data.Resource
import Data.Route
import Styles.Themes hiding (wait)

import Shared.Blog as Blog
import Shared.Types

import Pure.Elm.Application hiding (render,wait)
import Pure.Maybe

import Control.Concurrent.Async (wait)

import Control.Monad
import GHC.Exts (IsList(..))

newtype PostHeader = PostHeader PostView

toSpecificPost BlogR s = PostR s
toSpecificPost rt@PostR {} _ = rt
toSpecificPost (AuthorPostsR _) s = PostR s -- only global blog posts are shown on user pages, not per-package blogs
toSpecificPost (PackageBlogR pn) s = PackagePostR pn s
toSpecificPost rt@PackagePostR {} _ = rt
toSpecificPost _ _ = BlogR

instance Render (Route,PostHeader) where
  render (rt,PostHeader Post {..}) =
    Header <| Themed @HeaderT |> 
      [ render $ Avatar.Avatars (toList authors)
      , render $ Title.Title (toSpecificPost rt slug) (toTxt title)
      , render $ Subtitle.Subtitle subtitle 
      , render authors
      , render published
      , render tags
      ]

instance Render (ListItem PostView) where
  render (ListItem rt b p@Post {..}) =
    let 
      more = 
          [ Div <| Class "hide" 
          , Div <| Themed @MoreT |> [ A <| url Href Href (location (toSpecificPost rt slug)) |> [ "Read More >" ]]
          ]
    in article b (render (rt,PostHeader p)) (render excerpt) (render $ Markdown more)

instance Render (Route,(Request (Maybe PostView),Request (Maybe PostContentView))) where
  render (rt,(p,pcv)) =
    producing @(Maybe PostView) (either titled (wait >=> titled) p) 
      (consumingWith options (consumer True))
    where
      titled p = do
        case p of
          Nothing -> retitle "Not Found"
          Just Post {..} -> retitle (toTxt title)
        pure p

      consumer _ Nothing = notFound "Post"
      consumer b (Just p) = 
        article b (render (rt,PostHeader p)) (render (rt,pcv)) Null

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Just placeholderPostView) <| Themed @PlaceholderT)

instance Render (Route,Request (Maybe PostContentView)) where
  render (_,pcv) = 
    producing @(Maybe PostContentView) (either pure wait pcv) 
      (consumingWith options consumer)
    where
      consumer Nothing = Null
      consumer (Just (PostContent md)) = render md 

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer (Just placeholderPostContentView) <| Themed @PlaceholderT)

instance Render (Route,Request [PostView]) where
  render (rt,ps) = 
    producing @[PostView] (either pure wait ps) 
      (consumingWith options (consumer True id))
    where
      consumer b f ps = 
        Div <| Themed @HideT |>
          [ render (Listing b rt f (const Null) ps) 
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) [placeholderPostView])
