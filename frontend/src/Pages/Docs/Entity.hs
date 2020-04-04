module Pages.Docs.Entity (entity) where

import qualified App
import Data.Route as Route
import Components.Breadcrumbs (breadcrumbs)
import Pages.Docs.Styles
import Pages.Docs.PackageSearch (packageSearch)
import Styles.Themes

import qualified Shared.Cache as Cache
import qualified Shared.Doc as Doc

import Pure.Data.Try
import Pure.Elm.Application hiding (Hidden)

import Data.Bool
import Data.Maybe (listToMaybe,mapMaybe)

import Pure.Data.Txt (fromTxt)
import Pure.Data.JSON (pretty)
import Debug.Trace (trace)

entity :: App.App => Txt -> Txt -> Txt -> Txt -> Bool -> View
entity pkg ver mdl ent latest = let v = bool (Just ver) Nothing latest in
  Div <||>
    [ breadcrumbs (EntityR pkg v mdl ent)
    , Div <| Theme DocT |>
      case lookup (pkg,ver) (Cache.docs (App.cache session)) of
        Just (Done d)
          | (_,mdls) <- Doc.breakDoc d
          , Just m <- lookupModule mdl mdls
          , Just e <- lookupEntity ent m
          -> [ let Doc.Meta {..} = Doc.meta d
                in packageSearch package v [m]
             , Div <| Theme MarkdownT |> unhide e
             ]
        Just Trying -> [ "Loading entity." ]
        Just Failed -> [ "Entity not found." ]
        _ -> []
    ]

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

unhide = go
  where
    go [] = []
    go (Classes (elem "hide" -> True) Pre : Children cs Blockquote : xs) = cs ++ go xs
    go (x:xs) = x : go xs
