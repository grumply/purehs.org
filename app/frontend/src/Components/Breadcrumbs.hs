module Components.Breadcrumbs (breadcrumbs,sublinks) where

import qualified App
import Components.Preload ( prelink )
import Data.Route as Route
import Styles.Responsive ( largeScreens, (<%>) )

import Pure.Elm.Application

breadcrumbs :: App.App => Route -> [View]
breadcrumbs r =
  case r of

    BlogRoute r -> case r of
      BlogR                     -> b : []
      PostR _                   -> b : []

    TutorialRoute r -> case r of 
      TutorialsR                -> t : []
      TutorialR _               -> t : []

    AuthorRoute r -> case r of
      AuthorsR                  -> a : []
      AuthorR nm                -> a : chevron : n nm : []
      AuthorPackagesR nm        -> a : chevron : n nm : []
      AuthorTutorialsR nm       -> a : chevron : n nm : []
      AuthorBlogR nm            -> a : chevron : n nm : []

    PackageRoute r -> case r of
      PackagesR                 -> ps : []
      PackageR pn               -> ps : chevron : p pn : []
      PackageBlogR pn _         -> ps : chevron : p pn : []
      PackageVersionR pn ver    -> ps : chevron : p pn : chevron : v pn ver : []
      PackageTutorialR pn ver _ -> ps : chevron : p pn : chevron : v pn ver : []
      PackageModuleR pn ver _   -> ps : chevron : p pn : chevron : v pn ver : []
      PackageEntityR pn ver _ _ -> ps : chevron : p pn : chevron : v pn ver : []

    _                           -> []
  where
    chevron  = " ❯ " 
    ps       = A <| prelink (PackageRoute PackagesR) |> [ "Packages" ]
    p pn     = A <| prelink (PackageRoute (PackageR pn)) |> [ txt pn ] 
    v pn ver = A <| prelink (PackageRoute (PackageVersionR pn ver)) |> [ txt ver ]
    b        = A <| prelink (BlogRoute BlogR) |> [ "Blog" ]
    t        = A <| prelink (TutorialRoute TutorialsR) |> [ "Tutorials" ]
    a        = A <| prelink (AuthorRoute AuthorsR) |> [ "Authors" ]
    n nm     = A <| prelink (AuthorRoute (AuthorR nm)) |> [ txt nm ]

sublinks :: App.App => Route -> [View]
sublinks r =
  case r of
    AuthorRoute r -> case r of
      AuthorR n                 -> ps n : pkgs n : ts n : []
      AuthorPackagesR n         -> ps n : pkgs n : ts n : []
      AuthorTutorialsR n        -> ps n : pkgs n : ts n : []
      AuthorBlogR n             -> ps n : pkgs n : ts n : []
      _                         -> []

    PackageRoute r -> case r of
      PackagesR                 -> []
      PackageR pn               -> pb pn : []
      PackageBlogR pn _         -> pb pn : []
      PackageVersionR pn v      -> pb pn : pts pn v : []
      PackageTutorialR pn v _   -> pb pn : pts pn v : []
      PackageModuleR pn v mn    -> m pn v mn : []
      PackageEntityR pn v mn e  -> m pn v mn : []

    _                           -> []
  where
    ps n      = A <| prelink (AuthorRoute (AuthorBlogR n)) |> [ "Posts" ]
    pkgs n    = A <| prelink (AuthorRoute (AuthorPackagesR n)) |> [ "Packages" ]
    ts n      = A <| prelink (AuthorRoute (AuthorTutorialsR n)) |> [ "Tutorials" ]
    pb pn     = A <| prelink (PackageRoute (PackageBlogR pn BlogR)) |> [ "Blog" ]
    pts pn v  = A <| prelink (PackageRoute (PackageTutorialR pn v TutorialsR)) |> [ "Tutorials" ]
    m pn v mn = A <| Themed @Hideable . prelink (PackageRoute (PackageModuleR pn v mn)) |> [ txt mn ]

data Hideable
instance Theme Hideable where
  theme c =
    is c do
      display =: none

      -- small to medium screens are generally
      -- touch-capable and can use swipe here
      largeScreens <%> do
        display =: initial

