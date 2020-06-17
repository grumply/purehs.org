{-# language DuplicateRecordFields, UndecidableInstances, DeriveAnyClass #-}
module Main where

import App
import Components.Breadcrumbs as Breadcrumbs
import Components.Header as Header

import Pages.Author as Author
import Pages.Page as Page
import Pages.Post as Post
import Pages.Tutorial as Tutorial
import Pages.Package as Package

import qualified Pages.Home as Home

import Shared
import Shared.Types as Types
import Shared.Blog as Blog
import Shared.Tutorial as Tutorial
import Shared.Author as Author

import Styles.Themes as Themes hiding (wait)

import Data.Route
import Data.Render
import Data.Resource

import Pure.Data.Txt as Txt
import Pure.Data.Txt.Search
import Pure.Data.JSON (pretty)
import Pure (Pure(..))
import Pure.Elm.Application hiding (Session,Settings,content,page,wait,Async,title,render,Title)
import qualified Pure.Elm.Application as Elm
import Pure.Maybe
import qualified Pure.WebSocket as WS

import Control.Concurrent
import Control.Concurrent.Async (Async,async,wait)
import Data.Function ((&))
import Data.Functor
import Data.List as List
import Data.Typeable
import GHC.Exts (IsList(..))
import GHC.Generics
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ws <- WS.clientWS Shared.host Shared.port
  start <- time
  start `seq` WS.remote responseTimeAPI ws responseTest () (\_ -> time >>= \end -> print (end - start))
  inject body (Div <| Themed @AppT |> [ run (app ws) App.Settings ])
  where
    app ws = App [] [] [App.Routed] [] (App.mkSession ws) update Main.view

update :: Elm Message Route => Route -> Message -> Settings -> Session -> IO Session
update _rt (App.UpdateSession f) _ ses = 
  pure (f ses)

update _old (App.Routed _new) _ ses = do
  -- preload about/install
  case _new of
    HomeR -> void $ forkIO $ void $ do
      delay (Millisecond * 150)
      req ses getPage "about" 
      req ses getPageContent "about" 
      req ses getTutorial "install"
      req ses getTutorialContent "install"
    _ -> pure ()
  -- delay the restoration by a few frames
  async (delay (Millisecond * 100) >> addAnimation restoreScrollPosition) $> ses

view :: Route -> App.Page
view r = Elm.page $ 
  case r of
    -- Home page is a bit more custom.
    NoR -> Null
    HomeR -> Home.page
    _ -> 
      Themes.page $ 
        withHeader (Header.header r) $ 
          Div <||>
            [ case r of
                PageR _ -> Null
                _ -> 
                  Header <| Themed @PageHeaderT |> 
                    [ Nav <||> Breadcrumbs.breadcrumbs r 
                    , Nav <||> Breadcrumbs.sublinks r
                    ]
            , producingKeyed @Route r resource app
            ]

app :: App.App => Route -> Maybe Resource -> View
app rt Nothing = Null
app rt (Just rsc) = render (rt,rsc)

instance Render (Route,Resource) where
  render (rt,rsc) = 
    case rsc of
      PageResource pv pcv        -> render (rt,(pv,pcv))

      PackagesResource pvs       -> render (rt,pvs)
      PackageResource pv vs      -> render (rt,(pv,vs))

      TutorialsResource tvs      -> render (rt,tvs)
      TutorialResource tv tcv    -> render (rt,(tv,tcv))

      BlogResource pvs           -> render (rt,pvs)
      PostResource pv pcv        -> render (rt,(pv,pcv))

      AuthorsResource avs        -> render (rt,avs)
      AuthorResource av acv      -> render (rt,(av,acv))

      ModulesResource p vs ms    -> render (rt,(p,vs,ms))

      _                          -> Null