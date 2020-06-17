module Pages.Tutorial where

import qualified App
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

import Shared.Tutorial as Tutorial
import Shared.Types
import Shared

import Pure.Elm.Application hiding (render,wait)
import Pure.Maybe

import Control.Concurrent.Async (wait)

import Control.Monad
import Data.List as List
import Data.Maybe
import GHC.Exts (IsList(..))

newtype TutorialHeader = TutorialHeader TutorialView

toSpecificTutorial TutorialsR s = TutorialR s
toSpecificTutorial TutorialR {} s = TutorialR s
toSpecificTutorial (AuthorTutorialsR _) s = TutorialR s -- only glopbal tutorials are shown at the moment
toSpecificTutorial (VersionTutorialsR pn v) s = VersionTutorialR pn v s
toSpecificTutorial rt@VersionTutorialR {} _ = rt
toSpecificTutorial _ _ = TutorialsR -- fallback

instance Render (Route,TutorialHeader) where
  render (rt,TutorialHeader Tutorial {..}) =
    Header <| Themed @HeaderT |> 
      [ render $ Avatar.Avatars (toList authors)
      , render $ Title.Title (toSpecificTutorial rt slug) (toTxt title)
      , render $ Subtitle.Subtitle subtitle 
      , render authors
      , render published
      , render tags
      ]

instance Render (ListItem TutorialView) where
  render (ListItem rt b t@Tutorial {..}) =
    let 
      more = 
          [ Div <| Class "hide" 
          , Div <| Themed @MoreT |> [ A <| url Href Href (location (toSpecificTutorial rt slug)) |> [ "Read More >" ]]
          ]
    in
      article b (render (rt,TutorialHeader t)) (render excerpt) (render $ Markdown more)

instance Render (Route,(Request (Maybe TutorialView),Request (Maybe TutorialContentView))) where
  render (rt,(t,tcv)) =
    producing @(Maybe TutorialView) (either titled (wait >=> titled) t) 
      (consumingWith options (consumer True))
    where
      titled t = do
        case t of
          Nothing -> retitle "Not Found"
          Just Tutorial {..} -> retitle (toTxt title)
        pure t

      consumer _ Nothing = notFound "Tutorial"
      consumer b (Just tv)
        = Div <||> 
          [ article b (render (rt,TutorialHeader tv)) (render (rt,tcv)) Null
          , case series (tv :: TutorialView) of
              Just s | Nothing <- episode (tv :: TutorialView) -> 
                render (rt,s,App.req session Shared.listTutorials ())
              _ -> Null 
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Just placeholderTutorialView) <| Themed @PlaceholderT)


instance Render (Route,Request (Maybe TutorialContentView)) where
  render (_,tcv) = 
    producing @(Maybe TutorialContentView) (either pure wait tcv) 
      (consumingWith options consumer)
    where
      consumer Nothing = Null 
      consumer (Just (TutorialContent md)) = render md

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer (Just placeholderTutorialContentView) <| Themed @PlaceholderT)

instance Render (Route,Request [TutorialView]) where
  render (rt,tvs) = 
    producing @[TutorialView] (either pure wait tvs) 
      (consumingWith options (consumer True id))
    where
      consumer b f ts = 
        Div <| Themed @HideT . Themed @TutorialsT |>
          [ render (Listing b rt f (const Null) (List.filter (\t -> isNothing (episode (t :: TutorialView))) ts)) 
          ]

      options = defaultOptions 
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) [placeholderTutorialView])

instance Render (Route,Series,IO (Request [TutorialView])) where
  render (rt,s,tvs) =
    producing @[TutorialView] (tvs >>= either pure wait)
      (consumingWith options (consumer True))
    where
      consumer b ts = 
        let 
          match Tutorial {..} = series == Just s && isJust episode
          ts' = List.sort (List.filter match ts)
        in Div <| Themed @SubarticlesT |>
            ( H2 <| (if b then Themed @LoadT else id) |> [ "Series" ]
            : fmap (render . ListItem rt True) ts'
            )

      options = defaultOptions 
              & suspense (Milliseconds 500 0) 
                  (consumer False [placeholderTutorialView] <| Themed @PlaceholderT)