module Pages.Entity (page) where

import qualified App
import Data.Route as Route
import Control.Futures
import Components.Breadcrumbs (breadcrumbs)
import Components.Header (header)
import Components.PackageSearch (packageSearch)
import Styles.Themes

import qualified Shared.Doc as Doc
import Shared.Doc (Meta(..))

import Pure.Elm.Application hiding (Hidden,Meta,page,Frame)
import Pure.Spinners (ChasingDots(..))

import Data.Bool
import Data.Function ((&))
import Data.Maybe

page :: App.App => Txt -> Maybe Txt -> Txt -> Txt -> View
page pkg ver mdl ent 
  | Just v <- maybe (App.latest pkg) Just ver
  = Page $
      WithHeader (header (EntityR pkg ver mdl ent) False) $
        entity pkg v mdl ent (isNothing ver)

  | otherwise
  = Page $ WithHeader (header (EntityR pkg ver mdl ent) False) $
      consumingWith options consumer Nothing
  where
    consumer :: Maybe () -> View
    consumer _ = error "impossible"

    options = defaultOptions
            & suspense (Milliseconds 500 0) loading
            & trouble  (Seconds 5 0) problems

data Entity

entity :: App.App => Txt -> Txt -> Txt -> Txt -> Bool -> View
entity pkg ver mdl ent latest =
  producingKeyed @Entity (pkg,ver,mdl,ent,latest) 
    producer 
    (consumingWith options . consumer)
  where
    producer (p,v,_,_,_) = App.loadDoc (p,v)

    consumer = success

    options = defaultOptions 
            & suspense (Milliseconds 500 0) loading 
            & trouble  (Seconds 5 0) problems

loading :: View
loading = WithoutSidebar (Div <||> [ View (def @ChasingDots) ])

problems :: View
problems = WithoutSidebar "Problem loading package."

failed :: View
failed = WithoutSidebar "Entity not found."

success :: App.App => (Txt,Txt,Txt,Txt,Bool) -> Maybe Doc.Doc -> View
success (pkg,ver,mdl,ent,latest) doc
  | Just d    <- doc
  , (_,mdls)  <- Doc.breakDoc d
  , Just m    <- lookupModule mdl mdls
  , Just e    <- lookupEntity ent m
  , Meta {..} <- Doc.meta d
  , v         <- bool (Just ver) Nothing latest
  = WithHeader (breadcrumbs (EntityR pkg v mdl ent)) $
      WithSidebar (packageSearch package v [m]) $
        Div <| Theme MarkdownT . Theme UnhideT |> 
          unhide e

  | otherwise
  = failed

lookupModule :: Txt -> [[View]] -> Maybe [View]
lookupModule mdl = listToMaybe . mapMaybe isModule
  where
    isModule x@(Children [TextView _ m] H2 : _) | m == mdl = Just x
    isModule _ = Nothing

lookupEntity :: Txt -> [View] -> Maybe [View]
lookupEntity ent mdl
  | (_,x : xs) <- break match mdl, (y,_) <- break h3 xs = Just (x:y)
  | otherwise = Nothing
  where

    match (Children [TextView _ e] H3) | e == ent = True
    match _ = False

    h3 H3 = True
    h3 _  = False

unhide :: [View] -> [View]
unhide = go
  where
    go [] = []
    go (Classes (elem "hide" -> True) Pre : Children cs Blockquote : xs) = cs ++ go xs
    go (x:xs) = x : go xs
