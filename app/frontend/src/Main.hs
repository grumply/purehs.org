{-# language DuplicateRecordFields, UndecidableInstances, DeriveAnyClass #-}
module Main where

import App ( Session, Page, App, Message(..), Settings(..), mkSession )
import Components.Breadcrumbs as Breadcrumbs ( breadcrumbs, sublinks )
import Components.Header as Header ( header )

import Pages.Author as Author ()
import Pages.Page as Page ()
import Pages.Post as Post ()
import Pages.Tutorial as Tutorial ()
import Pages.Package as Package ()

import qualified Pages.Home as Home

import Shared ( host, port )

import Styles.Themes as Themes ( PageHeaderT, AppT, page, withHeader )

import Data.Route ( Route(..) )
import Data.Render ( Render(..) )
import Data.Resource

import Pure.Elm.Application hiding (Session,Settings,content,page,wait,Async,title,render,Title)
import qualified Pure.Elm.Application as Elm
import Pure.Maybe ( producingKeyed )
import qualified Pure.WebSocket as WS

import Control.Concurrent.Async (async)
import Data.Functor ( ($>) )
import System.IO ( stdout, hSetBuffering, BufferMode(LineBuffering) )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ws <- WS.clientWS Shared.host Shared.port
  inject body (Div <| Themed @AppT |> [ run (app ws) App.Settings ])
  where
    app ws = App [] [] [App.Routed] [] (App.mkSession ws) update Main.view

update :: Route -> Message -> Settings -> Session -> IO Session
update _rt (App.UpdateSession f) _ ses = 
  pure (f ses)

update _old (App.Routed _new) _ ses = do
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
app _rt Nothing = Null
app rt (Just rsc) = render (rt,rsc)

instance Render (Route,Resource) where
  render (rt,rsc) = 
    case rsc of
      PageResource pv pcv        -> render (rt,(pv,pcv))

      PackagesResource pvs       -> render (rt,pvs)
      PackageResource pv pc vs   -> render (rt,(pv,pc,vs))

      TutorialsResource tvs      -> render (rt,tvs)
      TutorialResource tv tcv    -> render (rt,(tv,tcv))

      BlogResource pvs           -> render (rt,pvs)
      PostResource pv pcv        -> render (rt,(pv,pcv))

      AuthorsResource avs        -> render (rt,avs)
      AuthorResource av acv      -> render (rt,(av,acv))

      ModulesResource p vs ms    -> render (rt,(p,vs,ms))

      _                          -> Null