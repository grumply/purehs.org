{-# language DuplicateRecordFields, UndecidableInstances, DeriveAnyClass, ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI , GHCForeignImportPrim #-}
module Main where

import App ( Session, Page, App, Message(..), Settings(..), mkSession, req )
import Cache ( cache )
import qualified Components.Author as Author
import qualified Components.Avatar as Avatar
import qualified Components.Breadcrumbs as Breadcrumbs
import qualified Components.Company as Company
import qualified Components.Description as Description
import qualified Components.Email as Email
import qualified Components.GitHubName as GitHubName
import qualified Components.Header as Header
import qualified Components.Markdown as Markdown
import qualified Components.Preload as Preload
import qualified Components.Published as Published
import qualified Components.Searcher as Searcher
import qualified Components.Subtitle as Subtitle
import qualified Components.Tags as Tags
import qualified Components.Title as Title
import qualified Components.Twitter as Twitter
import qualified Components.Version as Version
import Data.Entity
import Data.Placeholders
import Data.Route ( Route(..) )
import Data.Resource
import Styles.Themes as Themes

import qualified Pages.Home as Home

import qualified Shared
import Shared.Author as Author ( Author(..), AuthorContent(..) )
import Shared.Blog as Blog ( Post(..), PostContent(..) )
import Shared.Page as Page ( Page(..), PageContent(..) )
import Shared.Package as Package ( Package(..), PackageContent(..), Version(..), Module(..), ModuleContent(..) )
import Shared.Tutorial as Tutorial ( Tutorial(..), TutorialContent(..) )
import Shared.Types as Types ( Changes(Changes), Excerpt(Excerpt), Rendered(..) )

import Pure.Elm.Application hiding (Session,Settings,page,wait,Async,title,render,Title,black,gray,green,lavender,alpha,brightness)
import qualified Pure.Elm.Application as Pure
import Pure.Maybe ( consumingWith, defaultOptions, suspense, producing, producingKeyed )
import qualified Pure.WebSocket as WS

import Control.Concurrent.Async ( async, wait, Async )
import Control.Monad ( Monad((>>)), Functor(fmap), when )
import qualified Data.List as List
import Data.Maybe ( Maybe(..), isNothing, isJust )
import GHC.Exts (IsList(..))
import System.IO ( stdout, hSetBuffering, BufferMode(LineBuffering) )
import Prelude hiding (max,reverse)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ws <- WS.clientWS Shared.host Shared.port
  inject body (cache ws)
  inject body (Div <| Themed @Themes.App |> [ run (app ws) App.Settings ])
  where
    app ws = App [App.Startup] [] [App.Routed] [] (App.mkSession ws) update Main.view

update :: Route -> Message -> Settings -> Session -> IO Session
update _ App.Startup _ ses =
  pure ses

update _rt (App.UpdateSession f) _ ses = 
  pure (f ses)

update _old (App.Routed _new) _ ses = do
  -- delay the restoration by a few frames
  for_ (Pure.title _new) retitle 
  async (delay (Millisecond * 100) >> addAnimation restoreScrollPosition)
  case (_old,_new) of
    (NoR,HomeR) -> do
      addIdleWork do
        -- Pre-request the about and install pages if the visitor went directly to the home page
        -- This probably doesn't save any time since it is requested before mouseup on the links
        App.req Shared.backend Shared.getPage (fromTxt "about") 
        App.req Shared.backend Shared.getPageContent (fromTxt "about") 
        App.req Shared.backend Shared.getTutorial (fromTxt "install")
        App.req Shared.backend Shared.getTutorialContent (fromTxt "install")
        pure ()
      pure ()
    _ -> 
      pure ()
  pure ses


view :: Route -> App.Page
view r = Pure.page $ 
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
                  Header <| Themed @PageHeader |> 
                    [ Nav <||> Breadcrumbs.breadcrumbs r 
                    , Nav <||> Breadcrumbs.sublinks r
                    ]
            , producingKeyed @Route r resource app
            ]

app :: App.App => Route -> Maybe Resource -> View
app _rt Nothing = Null
app rt (Just rsc) = 
  case rsc of
    PageResource pv pcv      -> Main.page pv pcv
    PackagesResource pvs     -> Main.packages rt pvs
    PackageResource pv pc vs -> Main.package pv pc vs
    ModulesResource p vs ms  -> Main.documentation rt p vs ms
    TutorialsResource tvs    -> Main.tutorials rt tvs
    TutorialResource tv tcv  -> Main.tutorial rt tv tcv
    BlogResource pvs         -> Main.blog rt pvs
    PostResource pv pcv      -> Main.post rt pv pcv
    AuthorsResource avs      -> Main.authors avs
    AuthorResource av acv    -> Main.author av acv
    _                        -> Null

page :: App.App => Async (Maybe Page.Page) -> Async (Maybe (PageContent Rendered)) -> View
page ap apc = producing producer (consumingWith options consumer)
  where 
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder = 
          let ph = placeholderPageContentView
          in consumer (Just ph) <| Themed @Placeholder

    producer = do
      mp  <- wait ap
      mpc <- wait apc
      when (isNothing mp) do
        retitle "Not Found"
      pure mpc

    consumer (Just (PageContent md)) = Markdown.markdown md <| Themed @PageArticle
    consumer Nothing = notFound "Page"

data PageArticle
instance Theme PageArticle where
  theme c =
    is c do
      padding-top =: 60px
      padding-bottom =: 60px

packages :: App.App => Route -> Async [Package] -> View
packages rt aps = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = replicate 3 placeholderPackageView
          in consumer False ph <| Themed @Placeholder

    producer = do
      ps <- wait aps
      when (null ps) do
        retitle "Not Found"
      pure ps

    consumer loaded = Searcher.searcher listing
      where
        unblur | loaded = Themed @Load | otherwise = Themed @Placeholder

        listing v search xs =
          Div <| Themed @Searcher . unblur |>
            [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Packages"
            , Div <||> 
              [ Div <| Themed @Listing . Themed @Divided |>
                [ Div <||> -- flex space between
                  [ Div <||>
                    [ Author.author author
                    , " ❯ " 
                    , Title.title (PackageR name) (toTxt name)
                    ]
                  , Title.title (VersionR name latest) (toTxt latest)
                  ]
                , Tags.searchableTags tags search
                , Description.description description
                ]
              | Package {..} <- xs 
              ]
            ]

package :: App.App => Async (Maybe Package) -> Async (Maybe (PackageContent Rendered)) -> Async [Package.Version Rendered] -> View
package amp ampc avs = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = (placeholderPackageView,placeholderPackageContentView,[])
          in consumer False (Just ph) <| Themed @Placeholder

    producer = do
      mp  <- wait amp
      mpc <- wait ampc
      vs  <- wait avs
      when (null vs) do
        retitle "Not Found"
      pure do
        p  <- mp
        pc <- mpc
        pure (p,pc,vs)

    consumer :: Bool -> Maybe (Package,PackageContent Rendered,[Package.Version Rendered]) -> View
    consumer _ Nothing = notFound "Package"
    consumer loaded (Just (p@Package {..},PackageContent md,vs))
      | theme <- if loaded then Themed @Load else Themed @Placeholder
      = Div <| theme |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars (author : toList collaborators)
            , Title.title (PackageR name) (toTxt name)
            , Author.author author
            , Published.published published
            , Tags.tags tags 
            , Div <| Themed @Versions |>
              [ Version.version (v == latest) name v
              | v <- fmap Package.version vs
              ]
            ]
          , Markdown.markdown md
          ]
        , Div <| Themed @Subarticles |> 
          [ H2 <||> [ "Versions" ]
          , Div <||> 
            [ Article <| Themed @Article |> 
              [ Header <||> 
                [ Title.title (VersionR name version) (toTxt version)
                ]
              , Markdown.markdown cs
              ]
            | Package.Version {..} <- vs
            , let Changes cs = changes
            ]
          ]
        ]

documentation :: App.App => Route -> Async (Maybe Package) -> Async (Maybe (Package.Version Rendered)) -> Async [(Module Rendered,ModuleContent Rendered)] -> View
documentation rt amp amv amms =
  case rt of
    VersionR pn v     -> version pn v
    ModuleR pn v mn   -> _module pn v mn 
    EntityR pn v mn e -> entity pn v mn e
  where
    producer = do
      mp  <- wait amp
      mv  <- wait amv
      mms <- wait amms
      when (isNothing mp) do
        retitle "Not Found"
      pure do
        p  <- mp
        v  <- mv
        pure (p,v,mms)

    ph = (placeholderPackageView,placeholderVersionView,[])

    version pn v = producing producer (consumingWith options (consumer True))
      where
        options = defaultOptions & suspense (Milliseconds 500 0) placeholder
          where
            placeholder = consumer False (Just ph) <| Themed @Placeholder

        consumer loaded (Just (Package {..},Package.Version {..},ms)) 
          | theme <- if loaded then Themed @Load else Themed @Placeholder
          = Div <| theme |>
            [ Article <| Themed @Article |> 
              [ Header <| Themed @Header |>
                [ Avatar.avatars (author : toList collaborators)
                , Title.title (PackageR name) (toTxt name)
                , Author.author author
                , Published.published published
                , Tags.tags tags  
                , let t | v == latest = Themed @Latest | otherwise = id
                  in A <| Themed @Themes.Version . t . Preload.prelink (VersionR pn v) |> [ txt v ]
                ]
              , Description.description description
              ]
            , Div <| Themed @Subarticles |> 
              [ H2 <||> [ "Modules" ]
              , Div <||> 
                [ Article <| Themed @Article |> 
                  [ Header <||> 
                    [ Title.title (ModuleR pn v name) (toTxt name) ]
                  , Markdown.markdown md
                  ]
                | Module {..} <- fmap fst ms
                , let Excerpt md = excerpt
                ]
              ]
            ]
        consumer _ Nothing = notFound "Version"
              
    _module pn v mn = producing producer (consumingWith options (consumer True))
      where
        options = defaultOptions & suspense (Milliseconds 500 0) placeholder
          where
            placeholder = consumer False (Just ph) <| Themed @Placeholder

        consumer loaded (Just (Package {..},Package.Version {..},ms))
          | Just m <- List.find (\(Module {..},_) -> mn == name) ms
          , es     <- entities pn v m
          = Searcher.searcher listing es
          where
            unblur | loaded = Themed @Load | otherwise = Themed @Placeholder

            listing x search es = 
              Div <| Themed @Searcher . Themed @Hide . unblur |>
                [ Input <| Value x . OnInput (withInput search) . Placeholder "Search Module"
                , Div <| Themed @Subarticles |>
                  [ Div <| Themed @Article |>
                    [ Header <| Themed @Header |>
                      [ Title.title (EntityR pn v mn en) (toTxt en)
                      ]
                    , Markdown.markdown $ Rendered (vs' ++ more)
                    ]
                  | Entity ety en (EntityView vs) <- es
                  , let vs' = linkEntities pn v mn vs
                  , let more = [ Div <| Themed @More |> [ A <| url Href Href (location (EntityR pn v mn en)) |> [ "See More >" ]] ]
                  ]
                ]
        consumer _ _ = notFound "Module"
            
    entity pn v mn e = producing producer (consumingWith options (consumer True))
      where
        options = defaultOptions & suspense (Milliseconds 500 0) placeholder
          where
            placeholder = consumer False (Just ph) <| Themed @Placeholder
            
        consumer loaded (Just (Package {..},Package.Version {..},ms))
          | theme  <- if loaded then Themed @Load else Themed @Placeholder
          , Just m <- List.find (\(Module {..},_) -> mn == name) ms
          , es     <- entities pn v m
          , Just e <- List.find (\(Entity _ en _) -> e == en) es
          , Entity ety en (EntityView vs) <- rebaseEntityLinks pn v mn e
          = Div <| Themed @Subarticles . Themed @Unhide . theme |>
            [ Div <| Themed @Article |>
              [ Header <| Themed @Header |>
                [ Title.title (EntityR pn v mn en) (toTxt en)
                ]
              , Markdown.markdown $ Rendered vs
              ]
            ]
        consumer _ _ = notFound "Entity"

tutorials :: App.App => Route -> Async [Tutorial Rendered] -> View
tutorials rt ats = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = replicate 3 placeholderTutorialView
          in consumer False ph <| Themed @Placeholder

    producer = do
      ts <- wait ats
      when (null ts) do
        retitle "Not Found"
      pure ts
            
    consumer _ [] = emptyList "No tutorials, yet." Null
    consumer loaded ts = Searcher.searcher listing ts
      where
        unblur | loaded = Themed @Load | otherwise = Themed @Placeholder

        listing v search xs
          | ts <- List.filter (\t -> isNothing (episode (t :: Tutorial Rendered))) xs
          = Div <| Themed @Searcher . unblur |>
            [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Tutorials"
            , Div <||> 
              [ Div <| Themed @Listing |>
                [ Article <| Themed @Article |> 
                  [ Header <| Themed @Header |> 
                    [ Avatar.avatars (toList authors)
                    , Title.title t (toTxt title)
                    , Subtitle.subtitle subtitle 
                    , Author.authors authors
                    , Published.published published
                    , Tags.searchableTags tags search
                    ]
                  , Markdown.markdown e
                  , Markdown.markdown $ Rendered
                    [ Div <| Class "hide" 
                    , Div <| Themed @More |> [ A <| Preload.prelink t |> [ "Read More >" ]]
                    ]
                  ]
                ]
              | Tutorial {..} <- xs 
              , let t = toSpecificTutorial rt slug
              , let Excerpt e = excerpt
              ]
            ]     

toSpecificTutorial TutorialsR s = TutorialR s
toSpecificTutorial (TutorialR _) s = TutorialR s
toSpecificTutorial (AuthorTutorialsR _) s = TutorialR s -- only global tutorials are shown at the moment
toSpecificTutorial (VersionTutorialsR pn v) s = VersionTutorialR pn v s
toSpecificTutorial (VersionTutorialR pn v _) s = VersionTutorialR pn v s
toSpecificTutorial _ _ = TutorialsR -- fallback

tutorial :: App.App => Route -> Async (Maybe (Tutorial Rendered)) -> Async (Maybe (TutorialContent Rendered)) -> View
tutorial rt at atc = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = (placeholderTutorialView,placeholderTutorialContentView,Nothing)
          in consumer False (Just ph) <| Themed @Placeholder
          
    producer = do
      mt  <- wait at
      mtc <- wait atc
      mts <- 
        case mt of
          Just Tutorial {..} -> 
            case (series,episode) of
              -- If this is a series and not an episode in the series, 
              -- pull in all tutorials and find the ones in the series.
              -- It would be better if we could ask for just the episodes
              -- in the series, but the backend doesn't currently cache 
              -- series.
              (Just s,Nothing) -> do
                ts <- App.req Shared.backend Shared.listTutorials ()
                let 
                  inSeries Tutorial {..} = series == Just s
                  isEpisode Tutorial {..} = isJust episode
                  es = filter ((&&) <$> inSeries <*> isEpisode) ts
                pure (Just es)
              _ -> pure Nothing
          _ -> pure Nothing

      when (isNothing mt) do
        retitle "Not Found"

      pure do
        t  <- mt
        tc <- mtc
        pure (t,tc,mts)

    consumer :: Bool -> Maybe (Tutorial Rendered,TutorialContent Rendered,Maybe [Tutorial Rendered]) -> View
    consumer _ Nothing = notFound "Tutorial"
    consumer loaded (Just (Tutorial {..},TutorialContent md,mts))
      | theme <- if loaded then Themed @Load else Themed @Placeholder
      = Div <| theme |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars (toList authors)
            , Title.title rt (toTxt title)
            , Subtitle.subtitle subtitle 
            , Author.authors authors
            , Published.published published
            , Tags.tags tags 
            ]
          , Markdown.markdown md
          ]
        , case mts of
            Nothing -> Null

            -- When this is the tutorial series introduction, 
            -- display the series, too
            Just ts ->
              Div <| Themed @Subarticles |> 
                [ H2 <||> [ "Series" ]
                , Div <||> 
                  [ Article <| Themed @Article |> 
                    [ Header <| Themed @Header |>
                      [ Title.title t (toTxt title)
                      , Subtitle.subtitle subtitle 
                      , Author.authors authors
                      , Published.published published
                      , Tags.tags tags 
                      ]
                    , Markdown.markdown md
                    ]
                  | Tutorial.Tutorial {..} <- ts
                  , let Excerpt md = excerpt
                  , let t = toSpecificTutorial rt slug
                  ]
                ]
        ]

blog :: App.App => Route -> Async [Post Rendered] -> View
blog rt aps = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = replicate 3 placeholderPostView
          in consumer False ph <| Themed @Placeholder

    producer = do
      ps <- wait aps
      when (null ps) do
        retitle "Not Found"
      pure ps
      
    consumer _ [] = emptyList "No posts, yet." Null
    consumer loaded xs = Searcher.searcher listing xs
      where
        unblur | loaded = Themed @Load | otherwise = Themed @Placeholder

        listing v search xs
          | ts <- List.filter (\t -> isNothing (episode (t :: Post Rendered))) xs
          = Div <| Themed @Searcher . unblur |>
            [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Posts"
            , Div <||> 
              [ Div <| Themed @Listing |>
                [ Article <| Themed @Article |> 
                  [ Header <| Themed @Header |> 
                    [ Avatar.avatars (toList authors)
                    , Title.title t (toTxt title)
                    , Subtitle.subtitle subtitle 
                    , Author.authors authors
                    , Published.published published
                    , Tags.searchableTags tags search
                    ]
                  , Markdown.markdown e
                  , Markdown.markdown $ Rendered
                    [ Div <| Class "hide" 
                    , Div <| Themed @More |> [ A <| Preload.prelink t |> [ "Read More >" ]]
                    ]
                  ]
                ]
              | Post {..} <- xs 
              , let t = toSpecificPost rt slug
              , let Excerpt e = excerpt
              ]
            ]     

toSpecificPost BlogR s = PostR s
toSpecificPost (PostR _) s = PostR s
toSpecificPost (AuthorPostsR _) s = PostR s -- only global blog posts are shown on user pages, not per-package blogs
toSpecificPost (PackageBlogR pn) s = PackagePostR pn s
toSpecificPost rt@PackagePostR {} _ = rt
toSpecificPost _ _ = BlogR

post :: App.App => Route -> Async (Maybe (Post Rendered)) -> Async (Maybe (PostContent Rendered)) -> View
post rt ap apc = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = (placeholderPostView,placeholderPostContentView,Nothing)
          in consumer False (Just ph) <| Themed @Placeholder
          
    producer = do
      mp  <- wait ap
      mpc <- wait apc
      mps <- 
        case mp of
          Just Post {..} -> 
            case (series,episode) of
              -- If this is a series and not an episode in the series, 
              -- pull in all tutorials and find the ones in the series.
              -- It would be better if we could ask for just the episodes
              -- in the series, but the backend doesn't currently cache 
              -- series.
              (Just s,Nothing) -> do
                ts <- 
                  case rt of
                    PostR _ -> App.req Shared.backend Shared.listPosts ()
                    PackagePostR pn _ -> App.req Shared.backend Shared.listPackagePosts pn
                let 
                  inSeries Post {..} = series == Just s
                  isEpisode Post {..} = isJust episode
                  es = filter ((&&) <$> inSeries <*> isEpisode) ts
                pure (Just es)
              _ -> pure Nothing
          _ -> pure Nothing

      when (isNothing mp) do
        retitle "Not Found"

      pure do
        t  <- mp
        tc <- mpc
        pure (t,tc,mps)

    consumer :: Bool -> Maybe (Post Rendered,PostContent Rendered,Maybe [Post Rendered]) -> View
    consumer _ Nothing = notFound "Tutorial"
    consumer loaded (Just (Post {..},PostContent md,mts))
      | theme <- if loaded then Themed @Load else Themed @Placeholder
      = Div <| theme |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars (toList authors)
            , Title.title rt (toTxt title)
            , Subtitle.subtitle subtitle 
            , Author.authors authors
            , Published.published published
            , Tags.tags tags 
            ]
          , Markdown.markdown md
          ]
        , case mts of
            Nothing -> Null

            -- When this is the tutorial series introduction, 
            -- display the series, too
            Just ts ->
              Div <| Themed @Subarticles |> 
                [ H2 <||> [ "Series" ]
                , Div <||> 
                  [ Article <| Themed @Article |> 
                    [ Header <| Themed @Header |>
                      [ Title.title t (toTxt title)
                      , Subtitle.subtitle subtitle 
                      , Author.authors authors
                      , Published.published published
                      , Tags.tags tags 
                      ]
                    , Markdown.markdown md
                    ]
                  | Blog.Post {..} <- ts
                  , let Excerpt md = excerpt
                  , let t = toSpecificPost rt slug
                  ]
                ]
        ]

authors :: App.App => Async [Author Rendered] -> View
authors aas = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = replicate 3 placeholderAuthorView
          in consumer False ph <| Themed @Placeholder

    producer = do
      as <- wait aas
      when (null as) do
        retitle "Not Found"
      pure as
      
    consumer loaded = Searcher.searcher listing
      where
        unblur | loaded = Themed @Load | otherwise = Themed @Placeholder

        listing v search as = 
          Div <| Themed @Searcher . unblur |>
            [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Authors"
            , Div <||> 
              [ Div <| Themed @Listing |>
                [ Article <| Themed @Article |> 
                  [ Header <| Themed @Header |> 
                    [ Avatar.avatars [name]
                    , Title.title (AuthorR name) (toTxt name)
                    , maybe Null GitHubName.gitHubNameLink github
                    , maybe Null Twitter.twitterHandleLink twitter
                    , maybe Null Email.emailLink email
                    , maybe Null Company.companyLink company
                    ]
                  ]
                ]
              | Author {..} <- as
              ]
            ]     

author :: App.App => Async (Maybe (Author Rendered)) -> Async (Maybe (AuthorContent Rendered)) -> View
author aa aac = producing producer (consumingWith options (consumer True))
  where
    options = defaultOptions & suspense (Milliseconds 500 0) placeholder
      where
        placeholder =
          let ph = (placeholderAuthorView,placeholderAuthorContentView)
          in consumer False (Just ph) <| Themed @Placeholder
          
    producer = do
      ma  <- wait aa
      mac <- wait aac      
      when (isNothing ma) do
        retitle "Not Found"
      pure do
        a  <- ma
        ac <- mac
        pure (a,ac)

    consumer :: Bool -> Maybe (Author Rendered,AuthorContent Rendered) -> View
    consumer _ Nothing = notFound "Author"
    consumer loaded (Just (Author {..},AuthorContent md))
      | theme <- if loaded then Themed @Load else Themed @Placeholder
      = Div <| theme |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars [name]
            , Title.title (AuthorR name) (toTxt name)
            , maybe Null GitHubName.gitHubNameLink github
            , maybe Null Twitter.twitterHandleLink twitter
            , maybe Null Email.emailLink email
            , maybe Null Company.companyLink company
            ]
          , Markdown.markdown md
          ]
        ]
