{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Shared where

import Pure.Data.Txt

import Shared.Types
import qualified Shared.Author as Author
import qualified Shared.Blog as Blog
import qualified Shared.Package as Package
import qualified Shared.Page as Page
import qualified Shared.Tutorial as Tutorial

import Pure.WebSocket (API,api,mkRequest,(<:>),none,(<:+:>))

host :: String
host = "159.65.79.222"

port :: Int
port = 8081

mkRequest "ListAuthors" [t|() -> [Author.Author Rendered]|]
mkRequest "GetAuthor" [t|Name -> Maybe (Author.Author Rendered)|]
mkRequest "GetAuthorContent" [t|Name -> Maybe (Author.AuthorContent Rendered)|]
mkRequest "ListAuthorPackages" [t|Name -> [Package.Package]|]
mkRequest "ListAuthorPosts" [t|Name -> [Blog.Post Rendered]|]
mkRequest "ListAuthorTutorials" [t|Name -> [Tutorial.Tutorial Rendered]|]

authorAPI :: API '[] _
authorAPI = api msgs reqs
  where
    msgs = none
    reqs = listAuthors
       <:> getAuthor 
       <:> getAuthorContent
       <:> listAuthorPackages
       <:> listAuthorPosts
       <:> listAuthorTutorials
       <:> none



mkRequest "ListPages" [t|() -> [Page.Page]|]
mkRequest "GetPage" [t|Slug -> Maybe Page.Page|]
mkRequest "GetPageContent" [t|Slug -> Maybe (Page.PageContent Rendered)|]

pagesAPI :: API '[] _
pagesAPI = api msgs reqs
  where
    msgs = none
    reqs = listPages
       <:> getPage
       <:> getPageContent
       <:> none



mkRequest "ListPosts" [t|() -> [Blog.Post Rendered]|]
mkRequest "GetPost" [t|Slug -> Maybe (Blog.Post Rendered)|]
mkRequest "GetPostContent" [t|Slug -> Maybe (Blog.PostContent Rendered)|]

blogAPI :: API '[] _
blogAPI = api msgs reqs
  where
    msgs = none
    reqs = listPosts
       <:> getPost
       <:> getPostContent
       <:> none



mkRequest "ListTutorials" [t|() -> [Tutorial.Tutorial Rendered]|]
mkRequest "GetTutorial" [t|Slug -> Maybe (Tutorial.Tutorial Rendered)|]
mkRequest "GetTutorialContent" [t|Slug -> Maybe (Tutorial.TutorialContent Rendered)|]

tutorialAPI :: API '[] _
tutorialAPI = api msgs reqs
  where
    msgs = none
    reqs = listTutorials
       <:> getTutorial 
       <:> getTutorialContent
       <:> none



mkRequest "ListPackages" [t|() -> [Package.Package]|]
mkRequest "GetPackage" [t|PackageName -> Maybe Package.Package|]
mkRequest "GetPackageContent" [t|PackageName -> Maybe (Package.PackageContent Rendered)|]

packageAPI :: API '[] _
packageAPI = api msgs reqs
  where
    msgs = none
    reqs = listPackages
       <:> getPackage
       <:> getPackageContent
       <:> none



mkRequest "ListPackageVersions" [t|PackageName -> [Package.Version Rendered]|]
mkRequest "GetPackageVersion" [t|(PackageName,Version) -> Maybe (Package.Version Rendered)|]

packageVersionAPI :: API '[] _
packageVersionAPI = api msgs reqs
  where
    msgs = none
    reqs = listPackageVersions
       <:> getPackageVersion
       <:> none



mkRequest "ListPackagePosts" [t|PackageName -> [Blog.Post Rendered]|]
mkRequest "GetPackagePost" [t|(PackageName,Slug) -> Maybe (Blog.Post Rendered)|]
mkRequest "GetPackagePostContent" [t|(PackageName,Slug) -> Maybe (Blog.PostContent Rendered)|]

packageBlogAPI :: API '[] _
packageBlogAPI = api msgs reqs
  where
    msgs = none
    reqs = listPackagePosts
       <:> getPackagePost
       <:> getPackagePostContent
       <:> none



mkRequest "ListPackageVersionTutorials" [t|(PackageName,Version) -> [Tutorial.Tutorial Rendered]|]
mkRequest "GetPackageVersionTutorial" [t|(PackageName,Version,Slug) -> Maybe (Tutorial.Tutorial Rendered)|]
mkRequest "GetPackageVersionTutorialContent" [t|(PackageName,Version,Slug) -> Maybe (Tutorial.TutorialContent Rendered)|]

packageTutorialAPI :: API '[] _
packageTutorialAPI = api msgs reqs
  where
    msgs = none
    reqs = listPackageVersionTutorials
       <:> getPackageVersionTutorial
       <:> getPackageVersionTutorialContent
       <:> none



mkRequest "ListPackageVersionModules" [t|(PackageName,Version) -> [Package.Module Rendered]|]
mkRequest "ListPackageVersionModulesContent" [t|(PackageName,Version) -> [(Package.Module Rendered,Package.ModuleContent Rendered)]|]
mkRequest "GetPackageVersionModule" [t|(PackageName,Version,ModuleName) -> Maybe (Package.Module Rendered)|]
mkRequest "GetPackageVersionModuleContent" [t|(PackageName,Version,ModuleName) -> Maybe (Package.ModuleContent Rendered)|]

packageModuleAPI :: API '[] _
packageModuleAPI = api msgs reqs
  where
    msgs = none
    reqs = listPackageVersionModules
       <:> listPackageVersionModulesContent
       <:> getPackageVersionModule
       <:> getPackageVersionModuleContent
       <:> none


backend = 
  authorAPI <:+:> 
  pagesAPI <:+:> 
  blogAPI <:+:> 
  tutorialAPI <:+:> 
  packageAPI <:+:> 
  packageVersionAPI <:+:> 
  packageBlogAPI <:+:> 
  packageTutorialAPI <:+:> 
  packageModuleAPI


-- This is a bit naughty; I've inlined the API from try.purehs.org
-- without importing the package and the request type will match
-- up because the module name is the same `Shared`. Donn't do this!
mkRequest "Compile" [t|(Txt,Bool) -> Either Txt String|]
mkRequest "ReadModule" [t|String -> Maybe Txt|]

compileAPI :: API '[] '[Compile,ReadModule]
compileAPI = api msgs reqs
  where
    msgs = none
    reqs = compile <:> readModule <:> none
