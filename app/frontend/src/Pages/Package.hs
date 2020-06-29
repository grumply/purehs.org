module Pages.Package where

import qualified App as App
import Components.Article
import Components.Avatar as Avatar
import Components.Author as Author
import Components.CopyToClipboard
import Components.Markdown
import Components.Listing
import Components.Section as Section
import Components.Tags 
import Components.Title as Title
import Components.Time
import Components.Subtitle as Subtitle

import Data.Entity
import Data.Placeholders
import Data.Render
import Data.Resource
import Data.Route
import Styles.Colors
import Styles.Fonts
import Styles.Themes hiding (wait)
import Styles.Responsive

import Shared.Types as Types
import Shared.Package as Package

import Pure.Elm.Application hiding (render,wait,gray,black)
import Pure.Maybe

import Control.Concurrent.Async (wait)

import Control.Monad (join,(>=>))
import Data.List as List
import GHC.Exts (IsList(..))
import Prelude hiding (max)

instance Render (ListItem (Package Rendered)) where
  render (ListItem _ b searcher p@Package {..}) = 
    let loaded = if b then Themed @LoadT else id
    in Div <| Themed @PackageListingT . loaded |>
        [ Div <||> -- flex space between
          [ Div <||>
            [ Span <||> [ render (Author author) ] -- author name 
            , " ❯ " 
            , Span <||> [ render $ Title.Title (PackageR name) (toTxt name)  ] 
            ]
          , Div <||>
            [ Span <||> [ render $ Title.Title (VersionR name latest) (toTxt latest) ]
            ]
          ]
        , Div <||> [ render (tags,searcher) ]
        , P <||> [ txt description ]
        ]
    -- article b (render (p,[latest])) (txt description) Null

data PackageListingT
instance Theme PackageListingT where
  theme c = void $ 
    is c $ do
      apply $ do
        font-size =: 18px

      isn't lastChild .>
        border-bottom =* [1px,solid,toTxt (faded gray)]

      apply $ do
        padding     =* [0px,0px,32px,0px]
        margin      =* [0,auto]
        width       =: (100%)
        width       =: (100%)
        max-width   =: (100%)

      is firstOfType .> do
        margin-top  =: (-40)px

      mediumScreens <%> do
        width =: 700px

      largeScreens <%> do
        width =: 800px

      hugeScreens <%> do
        width =: 900px

      child (tag Div) .> do
        display =: flex
        justify-content =: space-between

      child (tag P) .> do
        font-family   =: defaultFont
        font-size     =: 18px
        color         =: toTxt black
        margin-top    =: 8px
        margin-bottom =: 0

      has (subtheme @TagsT) .> do
        margin =: 0
        margin-bottom =: 10px
        margin-left   =: 10px

      has (subtheme @AuthorT) $ do
        has (tag A) .> do
          font-size =: 20px

      has (subtheme @TitleT) $ do
        apply $ do
          display =: inline-block

        has (tag A) $ do
          apply $ do
            font-size =: 22px


data RelativeVersion = RelativeVersion PackageName Types.Version Types.Version

instance Render RelativeVersion where
  render (RelativeVersion pn latest v) = 
    let t | v == latest = Themed @LatestT | otherwise = id
    in A <| Themed @VersionT . t . link (VersionR pn v) |> [ txt v ]

data RelativeVersions = RelativeVersions PackageName Types.Version [Types.Version]

instance Render RelativeVersions  where
  render (RelativeVersions pn latest vs) =
    Div <| Themed @VersionsT |>
      [ render (RelativeVersion pn latest v)
      | v <- vs
      ]

instance Render (Package Rendered,[Types.Version]) where
  render (p@Package {..},vs) = 
    Header <| Themed @HeaderT |>
      [ render (Avatar.Avatars (author : toList collaborators))
      , render (Title.Title (PackageR name) (toTxt name))
      , render (Author.Author author)
      , render published
      , render tags  
      , render (RelativeVersions name latest vs)
      ]

instance Render (PackageName,Package.Version Rendered) where
  render (pn,Package.Version {..}) =
    Header <||> 
      [ render $ Title.Title (VersionR pn version) (toTxt version) ]

instance Render (Route,Request [Package.Version Rendered]) where
  render (PackageR pn,vs) =
    producing (either pure wait vs) 
      (consumingWith options (consumer True id))
    where
      consumer b f ms = Div <| Themed @SubarticlesT |> 
        ( H2 <| (if b then Themed @LoadT else id) |> [ "Versions" ]
        : [ article b (render (pn,v)) (render changes) Null <| f
          | v@Package.Version {..} <- ms 
          ]
        )

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) [placeholderVersionView])

instance Render (Route,(Request (Maybe (Package Rendered)),Request (Maybe (PackageContent Rendered)),Request [Package.Version Rendered])) where
  render (rt,(p,pc,vs)) =
    producing (either titled (wait >=> titled) p) 
      (consumingWith options (consumer True id))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ _ Nothing = notFound "Package"
      consumer b f (Just p@Package {..}) = 
        Div <||>
          [ article b (render (p,[latest])) (render (rt,pc)) Null <| f
          , render (rt,vs)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) (Just placeholderPackageView))

instance Render (Route,Request (Maybe (PackageContent Rendered))) where
  render (_,pcv) = 
    producing (either pure wait pcv) 
      (consumingWith options consumer)
    where
      consumer Nothing = Null
      consumer (Just (PackageContent md)) = render md 

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer (Just placeholderPackageContentView) <| Themed @PlaceholderT)

instance Render (Route,Request [Package Rendered]) where
  render :: App.App => (Route,Request [Package Rendered]) -> View
  render (rt,pvs) = 
    producing (either pure wait pvs) 
      (consumingWith options (consumer True id))
    where
      consumer b f = render . Listing b rt f (const Null) 

      options = defaultOptions 
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) [placeholderPackageView])

instance Render (PackageName,Types.Version,Module Rendered) where
  render (pn,v,Module {..}) =
    Header <||> 
      [ render $ Title.Title (ModuleR pn v name) (toTxt name) ]

instance Render (Route,Request [(Module Rendered,ModuleContent Rendered)]) where
  render (VersionR pn v,mvs) =
    producing (either pure wait mvs) 
      (consumingWith options (consumer True id . fmap fst))
    where
      consumer b f ms = Div <| Themed @SubarticlesT |> 
        ( H2 <| (if b then Themed @LoadT else id) |> [ "Modules" ]
        : [ article b (render (pn,v,m)) (render excerpt) Null <| f
          | m@Module {..} <- ms 
          ]
        )

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) [placeholderModuleView])

  render _ = Null

instance Render (PackageName,Request (Maybe (Package.Version Rendered))) where
  render (pn,vv) =
    producing (either titled (wait >=> titled) vv) 
      (consumingWith options (consumer True id))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ _ Nothing  = notFound "Version"
      consumer b f (Just v@Package.Version {..}) = 
        article b (render (pn,v)) (render changes) Null <| f

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) (Just placeholderVersionView))

instance Render (Route,(Request (Maybe (Package Rendered)),Request (Maybe (Package.Version Rendered)),Request [(Module Rendered,ModuleContent Rendered)])) where
  render (r@(VersionR pn v),(pv,vv,mvs)) =
    Tagged @(Package.Version Rendered) $
      producing (either titled (wait >=> titled) pv) 
        (consumingWith options (consumer True id))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ f Nothing = notFound "Version"
      consumer b f (Just p@Package {..}) = 
        Div <||>
          [ article b (render (p,[latest])) (txt description) Null <| f
          , render (r,mvs)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) (Just placeholderPackageView))

  render (ModuleR pn v mn,(pv,vv,mvs)) = 
    Tagged @(Module Rendered) $
      producing (either pure wait mvs)
        (consumingWith options (consumer True id . findModule))
    where
      findModule :: [(Module Rendered,ModuleContent Rendered)] -> Maybe [Entity]
      findModule = fmap (entities pn v) . List.find (\(Module {..},_) -> mn == name) 

      consumer :: Bool -> (View -> View) -> Maybe [Entity] -> View
      consumer _ _ Nothing = notFound "Module"
      consumer b f (Just es) = 
        Div <| Themed @HideT . Themed @SubarticlesT |>
          [ render (Listing b (ModuleR pn v mn) f (\es -> render (pn,v,mn,es)) es)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) (Just placeholderEntities))

  render (EntityR pn v mn e,(pv,vv,mvs)) =
    Tagged @Entity $
      producing (either pure wait mvs)
        (consumingWith options (consumer True id . findEntity . findModule))
    where
      findModule :: [(Module Rendered,ModuleContent Rendered)] -> Maybe [Entity]
      findModule = fmap (entities pn v) . List.find (\(Module {..},_) -> mn == name) 

      findEntity :: Maybe [Entity] -> Maybe Entity
      findEntity = join . fmap (List.find (\(Entity _ en _) -> e == en))

      consumer :: Bool -> (View -> View) -> Maybe Entity -> View
      consumer _ _ Nothing = notFound "Entity"
      consumer b f (Just et) = 
        Div <| Themed @UnhideT . Themed @SubarticlesT |> 
          [ render (ListItem (EntityR pn v mn e) b (const (pure ())) et)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) (Just (List.head placeholderEntities)))

instance Render (PackageName,Types.Version,ModuleName,Entity) where
  render (pn,v,mn,Entity ety en (EntityView vs)) = 
    let vs' = linkEntities pn v mn vs
        more = [ Div <| Themed @MoreT |> [ A <| url Href Href (location (EntityR pn v mn en)) |> [ "See More >" ]] ]
    in article True (render (pn,v,mn,en)) (render $ Rendered (vs' ++ more)) Null

instance Render (PackageName,Types.Version,ModuleName,[Entity]) where
  render (pn,v,mn,es) = Div <||> fmap (\e -> render (pn,v,mn,e)) es

instance Render (ListItem Entity) where
  render (ListItem (ModuleR pn v mn) _ _ (Entity ety en (EntityView vs))) =
    let vs' = linkEntities pn v mn vs
        more = [ Div <| Themed @MoreT |> [ A <| url Href Href (location (EntityR pn v mn en)) |> [ "See More >" ]] ]
    in article True (render (pn,v,mn,en)) (render $ Rendered (vs' ++ more)) Null

  render (ListItem (EntityR pn v mn _) _ _ e) =
    let Entity ety en (EntityView vs') = rebaseEntityLinks pn v mn e
    in article True (render (pn,v,mn,en)) (render $ Rendered vs') Null

  render _ = Null

instance Render (PackageName,Types.Version,ModuleName,Txt) where
  render (pn,v,mn,en) =
    Header <| Themed @HeaderT |>
      [ render $ Title.Title (EntityR pn v mn en) (toTxt en)
      ]
