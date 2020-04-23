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

data Module

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
  producingKeyed @Module (pkg,ver,mdl,latest) producer 
    (consumingWith options . consumer)
  where

    producer (p,v,_,_) = App.loadDoc (p,v)

    consumer = success

    options = defaultOptions 
            & suspense (Milliseconds 500 0) loading 
            & trouble  (Seconds 5 0) problems

loading :: View
loading = WithoutSidebar (Div <||> [ View (def @ChasingDots) ])

problems :: View
problems = WithoutSidebar "Problem loading module."

success :: App.App => (Txt,Txt,Txt,Bool) -> Maybe Doc.Doc -> View
success (p,v,m,l) md
  | Just d <- md
  = doc m d l

  | otherwise 
  = failed

failed :: View
failed = WithoutSidebar "Module not found."

doc :: App.App => Txt -> Doc -> Bool -> View
doc mdl d latest 
  | (_,(md:_)) <- breakDoc d
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

