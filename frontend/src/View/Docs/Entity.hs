module View.Docs.Entity where

import Themes
import Types
import View.Docs.Breadcrumbs (breadcrumbs)
import View.Docs.Searcher (searcher)

import qualified Shared.Cache as Cache
import qualified Shared.Doc as Doc

import Pure.Data.Try
import Pure.Elm

import Data.Maybe (listToMaybe,mapMaybe)
import Debug.Trace

entity :: Model -> Txt -> Txt -> Txt -> Txt -> View
entity model pkg ver mdl ent
  = Div <||>
      [ breadcrumbs (route model)
      , Div <| Theme DocT |>
        case lookup (pkg,ver) (Cache.docs (cache model)) of
          Just (Done d)
            | (_,mdls) <- Doc.breakDoc d
            , Just m <- lookupModule mdl mdls
            , Just e <- lookupEntity ent m
            -> [ let Doc.Meta {..} = Doc.meta d
                  in searcher package version mdls
               , Div <| Theme MarkdownT |> e
               ]
          _ -> [ "Entity not found." ]
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

    match (Children [Children [ TextView _ e ] Code] H3) | e == ent = True
    match _ = False

    h3 H3 = True
    h3 _  = False