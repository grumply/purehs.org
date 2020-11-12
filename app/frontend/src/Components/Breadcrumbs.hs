module Components.Breadcrumbs (breadcrumbs,sublinks) where

import qualified App
import Components.Preload ( prelink )
import Data.Route as Route
import Styles.Responsive ( largeScreens, (<%>) )

import Pure.Elm.Application

breadcrumbs :: App.App => Route -> [View]
breadcrumbs r =
  case r of

    BlogR                     -> b : []
    PostR _                   -> b : []

    TutorialsR                -> t : []
    TutorialR _               -> t : []

    AuthorsR                  -> a : []
    AuthorR nm                -> a : chevron : n nm : []
    AuthorPackagesR nm        -> a : chevron : n nm : []
    AuthorTutorialsR nm       -> a : chevron : n nm : []
    AuthorPostsR nm           -> a : chevron : n nm : []

    PackagesR                 -> ps : []
    PackageR pn               -> ps : chevron : p pn : []
    PackageBlogR pn           -> ps : chevron : p pn : []
    PackagePostR pn _         -> ps : chevron : p pn : []
    VersionR pn ver           -> ps : chevron : p pn : chevron : v pn ver : []
    VersionTutorialsR pn ver  -> ps : chevron : p pn : chevron : v pn ver : []
    VersionTutorialR pn ver _ -> ps : chevron : p pn : chevron : v pn ver : []
    ModuleR pn ver _          -> ps : chevron : p pn : chevron : v pn ver : []
    EntityR pn ver _ _        -> ps : chevron : p pn : chevron : v pn ver : []

    _                         -> []
  where
    chevron  = " ❯ " 
    ps       = A <| prelink PackagesR |> [ "Packages" ]
    p pn     = A <| prelink (PackageR pn) |> [ txt pn ] 
    v pn ver = A <| prelink (VersionR pn ver) |> [ txt ver ]
    b        = A <| prelink BlogR |> [ "Blog" ]
    t        = A <| prelink TutorialsR |> [ "Tutorials" ]
    a        = A <| prelink AuthorsR |> [ "Authors" ]
    n nm     = A <| prelink (AuthorR nm) |> [ txt nm ]

sublinks :: App.App => Route -> [View]
sublinks r =
  case r of
    AuthorR n               -> ps n : pkgs n : ts n : []
    AuthorPackagesR n       -> ps n : pkgs n : ts n : []
    AuthorTutorialsR n      -> ps n : pkgs n : ts n : []
    AuthorPostsR n          -> ps n : pkgs n : ts n : []
    EntityR pn v mn _       -> m pn v mn : []
    PackageR pn             -> pb pn : []
    PackagePostR pn _       -> pb pn : []
    PackageBlogR pn         -> pb pn : []
    VersionTutorialsR pn v  -> pb pn : pts pn v : []
    VersionTutorialR pn v _ -> pb pn : pts pn v : []
    VersionR pn v           -> pb pn : pts pn v : []
    _                       -> []
  where
    ps n      = A <| prelink (AuthorPostsR n)     |> [ "Posts" ]
    pkgs n    = A <| prelink (AuthorPackagesR n)  |> [ "Packages" ]
    ts n      = A <| prelink (AuthorTutorialsR n) |> [ "Tutorials" ]
    pb pn     = A <| prelink (PackageBlogR pn)    |> [ "Blog" ]
    pts pn v  = A <| prelink (VersionTutorialsR pn v) |> [ "Tutorials" ]
    m pn v mn = A <| Themed @Hideable . prelink (ModuleR pn v mn) |> [ txt mn ]

data Hideable
instance Theme Hideable where
  theme c =
    is c do
      display =: none

      -- small to medium screens are generally
      -- touch-capable and can use swipe here
      largeScreens <%> do
        display =: initial

