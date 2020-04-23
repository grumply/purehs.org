module Pages.Module (page) where

import qualified App
import Control.Futures
import Components.Breadcrumbs (breadcrumbs)
import Components.Header (header)
import Components.PackageSearch (packageSearch)
import Data.Route as Route
import Styles.Themes

import Shared.Doc as Doc (Doc(meta),Meta(..),breakDoc)

import Pure.Data.Txt as Txt
import Pure.Elm.Application hiding (Doc,Meta,page,Frame)
import Pure.Spinners (ChasingDots(..))

import Data.Bool
import Data.List as List
import Data.Function ((&))
import Data.Maybe

page :: App.App => Txt -> Maybe Txt -> Txt -> View
page pkg ver mdl 
  | Just v <- maybe (App.latest pkg) Just ver
  = Page $
      WithHeader (header (ModuleR pkg ver mdl) False) $
        packageModule pkg v mdl (isNothing ver)
  
  | otherwise
  = Page $ WithHeader (header (ModuleR pkg ver mdl) False) $
      consumingWith options consumer Nothing
  where
    consumer :: Maybe () -> View
    consumer _ = error "impossible"

    options = defaultOptions
            & suspense (Milliseconds 500 0) loading
            & trouble  (Seconds 5 0) problems

packageModule :: App.App => Txt -> Txt -> Txt -> Bool -> View
packageModule pkg ver mdl latest = 
  producingKeyed @Doc.Doc (pkg,ver) producer 
    (consumingWith options . consumer)
  where

    producer = App.loadDoc

    consumer = success mdl latest

    options = defaultOptions 
            & suspense (Milliseconds 500 0) loading 
            & trouble  (Seconds 5 0) problems

loading :: View
loading = WithoutSidebar (Div <||> [ View (def @ChasingDots) ])

problems :: View
problems = WithoutSidebar "Problem loading module."

success :: App.App => Txt -> Bool -> (Txt,Txt) -> Maybe Doc.Doc -> View
success m l (p,v) md
  | Just d <- md
  = doc m d l

  | otherwise 
  = failed

failed :: View
failed = WithoutSidebar "Module not found."

doc :: App.App => Txt -> Doc -> Bool -> View
doc mdl d latest 
  | (_,mds)    <- breakDoc d
  , (md:_)     <- findModule mdl mds
  , Meta {..}  <- Doc.meta d 
  , v          <- bool (Just version) Nothing latest
  = WithHeader (breadcrumbs (ModuleR package v mdl)) $
      WithSidebar (packageSearch "Module" package v [md]) $
        Div <| Theme MarkdownT |> 
          [ processLinks e
          | e <- linkEntities package v mdl (safeTail md)
          ]
  | otherwise = failed
  where
    safeTail [] = []
    safeTail (_ : xs) = xs

findModule :: Txt -> [[View]] -> [[View]]
findModule mdl = List.filter match
  where
    match m@(Children [TextView _ t] H2 : _) = t == mdl
    match _ = False

linkEntities pkg ver mdl = go
  where
    go [] = []
    go (Children [t@(TextView _ e)] H3 : rest) = H3 <||> [ A <| url Href Href (location (EntityR pkg ver mdl e)) |> [ t ] ] : go rest
    go (x : xs) = x : go xs

