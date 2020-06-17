module Pages.Package where

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
import Styles.Themes hiding (wait)

import Shared.Types as Types
import Shared.Package as Package

import Pure.Elm.Application hiding (render,wait)
import Pure.Maybe

import Control.Concurrent.Async (wait)

import Control.Monad (join,(>=>))
import Data.List as List
import GHC.Exts (IsList(..))

instance Render (ListItem PackageView) where
  render (ListItem _ b p@Package {..}) =
    article b (render (p,[latest])) (render excerpt) Null

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

instance Render (PackageView,[Types.Version]) where
  render (p@Package {..},vs) = 
    Header <| Themed @HeaderT |>
      [ render (Avatar.Avatars (author : toList collaborators))
      , render (Title.Title (PackageR name) (toTxt name))
      , render (Author.Author author)
      , render published
      , render tags  
      , render (RelativeVersions name latest vs)
      ]

instance Render (PackageName,VersionView) where
  render (pn,Package.Version {..}) =
    Header <||> 
      [ render $ Title.Title (VersionR pn version) (toTxt version) ]

instance Render (Route,Request [VersionView]) where
  render (PackageR pn,vs) =
    producing @[VersionView] (either pure wait vs) 
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

instance Render (Route,(Request (Maybe PackageView),Request [VersionView])) where
  render (rt,(p,vs)) =
    producing @(Maybe PackageView) (either titled (wait >=> titled) p) 
      (consumingWith options (consumer True id))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ _ Nothing = notFound "Package"
      consumer b f (Just p@Package {..}) = 
        Div <||>
          [ article b (render (p,[latest])) (render excerpt) Null <| f
          , render (rt,vs)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) (Just placeholderPackageView))

instance Render (Route,Request [PackageView]) where
  render (rt,pvs) = 
    producing @[PackageView] (either pure wait pvs) 
      (consumingWith options (consumer True id))
    where
      consumer b f = render . Listing b rt f (const Null)

      options = defaultOptions 
              & suspense (Milliseconds 500 0) 
                  (consumer False (Themed @PlaceholderT) [placeholderPackageView])

instance Render (PackageName,Types.Version,ModuleView) where
  render (pn,v,Module {..}) =
    Header <||> 
      [ render $ Title.Title (ModuleR pn v name) (toTxt name) ]

instance Render (Route,Request [(ModuleView,ModuleContentView)]) where
  render (VersionR pn v,mvs) =
    producing @[(ModuleView,ModuleContentView)] (either pure wait mvs) 
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

instance Render (PackageName,Request (Maybe VersionView)) where
  render (pn,vv) =
    producing @(Maybe VersionView) (either titled (wait >=> titled) vv) 
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

instance Render (Route,(Request (Maybe PackageView),Request (Maybe VersionView),Request [(ModuleView,ModuleContentView)])) where
  render (r@(VersionR pn v),(pv,vv,mvs)) =
    producing @(Maybe PackageView) (either titled (wait >=> titled) pv) 
      (consumingWith options (consumer True id))
    where
      titled Nothing = retitle "Not Found" >> pure Nothing
      titled x       = pure x

      consumer _ f Nothing = notFound "Version"
      consumer b f (Just p@Package {..}) = 
        Div <||>
          [ article b (render (p,[latest])) (render excerpt) Null <| f
          , render (r,mvs)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) (Just placeholderPackageView))

  render (ModuleR pn v mn,(pv,vv,mvs)) = 
    producing @[(ModuleView,ModuleContentView)] (either pure wait mvs)
      (consumingWith options (consumer True id . findModule))
    where
      findModule :: [(ModuleView,ModuleContentView)] -> Maybe [Entity]
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
    producing @[(ModuleView,ModuleContentView)] (either pure wait mvs)
      (consumingWith options (consumer True id . findEntity . findModule))
    where
      findModule :: [(ModuleView,ModuleContentView)] -> Maybe [Entity]
      findModule = fmap (entities pn v) . List.find (\(Module {..},_) -> mn == name) 

      findEntity :: Maybe [Entity] -> Maybe Entity
      findEntity = join . fmap (List.find (\(Entity _ en _) -> e == en))

      consumer :: Bool -> (View -> View) -> Maybe Entity -> View
      consumer _ _ Nothing = notFound "Entity"
      consumer b f (Just et) = 
        Div <| Themed @UnhideT . Themed @SubarticlesT |> 
          [ render (ListItem (EntityR pn v mn e) b et)
          ]

      options = defaultOptions
              & suspense (Milliseconds 500 0)
                  (consumer False (Themed @PlaceholderT) (Just (List.head placeholderEntities)))

instance Render (PackageName,Types.Version,ModuleName,Entity) where
  render (pn,v,mn,Entity ety en (EntityView vs)) = 
    let vs' = linkEntities pn v mn vs
        more = [ Div <| Themed @MoreT |> [ A <| url Href Href (location (EntityR pn v mn en)) |> [ "See More >" ]] ]
    in article True (render (pn,v,mn,en)) (render $ Markdown (vs' ++ more)) Null

instance Render (PackageName,Types.Version,ModuleName,[Entity]) where
  render (pn,v,mn,es) = Div <||> fmap (\e -> render (pn,v,mn,e)) es

instance Render (ListItem Entity) where
  render (ListItem (ModuleR pn v mn) _ e) = render (pn,v,mn,e)
  render (ListItem (EntityR pn v mn _) _ e) = render (pn,v,mn,e)
  render _ = Null

instance Render (PackageName,Types.Version,ModuleName,Txt) where
  render (pn,v,mn,en) =
    Header <| Themed @HeaderT |>
      [ render $ Title.Title (EntityR pn v mn en) (toTxt en)
      ]
