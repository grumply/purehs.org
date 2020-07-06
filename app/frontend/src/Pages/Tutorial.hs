module Pages.Tutorial where

import qualified App
import Components.Article
import Components.Avatar as Avatar
import Components.Author as Author
import Components.Markdown
import Components.Listing
import Components.Preload
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

newtype TutorialHeader = TutorialHeader (Tutorial Rendered)

toSpecificTutorial TutorialsR s = TutorialR s
toSpecificTutorial TutorialR {} s = TutorialR s
toSpecificTutorial (AuthorTutorialsR _) s = TutorialR s -- only glopbal tutorials are shown at the moment
toSpecificTutorial (VersionTutorialsR pn v) s = VersionTutorialR pn v s
toSpecificTutorial rt@VersionTutorialR {} _ = rt
toSpecificTutorial _ _ = TutorialsR -- fallback

instance Render (Route,TutorialHeader,Maybe (Txt -> IO ())) where
  render (rt,TutorialHeader Tutorial {..},searcher) =
    Header <| Themed @HeaderT |> 
      [ render $ Avatar.Avatars (toList authors)
      , render $ Title.Title (toSpecificTutorial rt slug) (toTxt title)
      , render $ Subtitle.Subtitle subtitle 
      , render authors
      , render published
      , maybe (render tags) (\s -> render (tags,s)) searcher
      ]

instance Render (ListItem (Tutorial Rendered)) where
  render (ListItem rt b searcher t@Tutorial {..}) =
    let 
      r = toSpecificTutorial rt slug
      more = 
          [ Div <| Class "hide" 
          , Div <| Themed @MoreT |> [ A <| prelink r |> [ "Read More >" ]]
          ]
    in
      article b (render (rt,TutorialHeader t,Just searcher)) (render excerpt) (render $ Rendered more)

instance Render (Route,(Request (Maybe (Tutorial Rendered)),Request (Maybe (TutorialContent Rendered)))) where
  render (rt,(t,tcv)) =
    producing (either titled (wait >=> titled) t) 
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
          [ article b (render (rt,TutorialHeader tv,Nothing @(Txt -> IO ()))) (render (rt,tcv)) Null
          , case series (tv :: Tutorial Rendered) of
              Just s | Nothing <- episode (tv :: Tutorial Rendered) -> 
                render (rt,s,App.req session Shared.listTutorials ())
              _ -> Null 
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Just placeholderTutorialView) <| Themed @PlaceholderT)


instance Render (Route,Request (Maybe (TutorialContent Rendered))) where
  render (_,tcv) = 
    producing (either pure wait tcv) 
      (consumingWith options consumer)
    where
      consumer Nothing = Null 
      consumer (Just (TutorialContent md)) = render md

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer (Just placeholderTutorialContentView) <| Themed @PlaceholderT)

instance Render (Route,Request [Tutorial Rendered]) where
  render (rt,tvs) = 
    Tagged @[Tutorial Rendered] $
      producing (either pure wait tvs) 
        (consumingWith options (consumer True id))
    where
      consumer b _ [] = emptyList "No Tutorials Yet" Null
      consumer b f ts = 
        Div <| Themed @HideT . Themed @TutorialsT |>
          [ render (Listing b rt f (const Null) (List.filter (\t -> isNothing (episode (t :: Tutorial Rendered))) ts)) 
          ]

      options = defaultOptions 
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) [placeholderTutorialView])

instance Render (Route,Series,IO (Request [Tutorial Rendered])) where
  render (rt,s,tvs) =
    Tagged @Series $
      producing (tvs >>= either pure wait)
        (consumingWith options (consumer True))
    where
      consumer b [] = emptyList "No Tutorials Yet" Null
      consumer b ts = 
        let 
          match Tutorial {..} = series == Just s && isJust episode
          ts' = List.sort (List.filter match ts)
        in Div <| Themed @SubarticlesT |>
            ( H2 <| (if b then Themed @LoadT else id) |> [ "Series" ]
            : fmap (render . ListItem rt True (const (pure ()))) ts'
            )

      options = defaultOptions 
              & suspense (Milliseconds 500 0) 
                  (consumer False [placeholderTutorialView] <| Themed @PlaceholderT)