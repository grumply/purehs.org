{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Shared where

import Pure.Data.Txt

import Shared.Types
import qualified Shared.Author as Author
import qualified Shared.Blog as Blog
import qualified Shared.Package as Package
import qualified Shared.Page as Page
import qualified Shared.Tutorial as Tutorial

import Pure.WebSocket (mkRequest,(<:>),none,(<:+:>))
import qualified Pure.WebSocket as WS (FullAPI,api)

host :: String
-- host = "159.65.79.222"
host = "192.168.1.8"

port :: Int
port = 8081

mkRequest "ListAuthors" [t|() -> [Author.AuthorView]|]
mkRequest "GetAuthor" [t|Name -> Maybe Author.AuthorView|]
mkRequest "GetAuthorContent" [t|Name -> Maybe Author.AuthorContentView|]
mkRequest "ListAuthorPackages" [t|Name -> [Package.PackageView]|]
mkRequest "ListAuthorPosts" [t|Name -> [Blog.PostView]|]
mkRequest "ListAuthorTutorials" [t|Name -> [Tutorial.TutorialView]|]

authorAPI :: WS.FullAPI '[] _
authorAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listAuthors
       <:> getAuthor 
       <:> getAuthorContent
       <:> listAuthorPackages
       <:> listAuthorPosts
       <:> listAuthorTutorials
       <:> none



mkRequest "ListPages" [t|() -> [Page.PageView]|] -- probably not used
mkRequest "GetPage" [t|Slug -> Maybe Page.PageView|]
mkRequest "GetPageContent" [t|Slug -> Maybe Page.PageContentView|]

pagesAPI :: WS.FullAPI '[] _
pagesAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPages
       <:> getPage
       <:> getPageContent
       <:> none



mkRequest "ListPosts" [t|() -> [Blog.PostView]|]
mkRequest "GetPost" [t|Slug -> Maybe Blog.PostView|]
mkRequest "GetPostContent" [t|Slug -> Maybe Blog.PostContentView|]

blogAPI :: WS.FullAPI '[] _
blogAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPosts
       <:> getPost
       <:> getPostContent
       <:> none



mkRequest "ListTutorials" [t|() -> [Tutorial.TutorialView]|]
mkRequest "GetTutorial" [t|Slug -> Maybe Tutorial.TutorialView|]
mkRequest "GetTutorialContent" [t|Slug -> Maybe Tutorial.TutorialContentView|]

tutorialAPI :: WS.FullAPI '[] _
tutorialAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listTutorials
       <:> getTutorial 
       <:> getTutorialContent
       <:> none



mkRequest "ListPackages" [t|() -> [Package.PackageView]|]
mkRequest "GetPackage" [t|PackageName -> Maybe Package.PackageView|]

packageAPI :: WS.FullAPI '[] _
packageAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPackages
       <:> getPackage
       <:> none



mkRequest "ListPackageVersions" [t|PackageName -> [Package.VersionView]|]
mkRequest "GetPackageVersion" [t|(PackageName,Version) -> Maybe Package.VersionView|]

packageVersionAPI :: WS.FullAPI '[] _
packageVersionAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPackageVersions
       <:> getPackageVersion
       <:> none



mkRequest "ListPackagePosts" [t|PackageName -> [Blog.PostView]|]
mkRequest "GetPackagePost" [t|(PackageName,Slug) -> Maybe Blog.PostView|]
mkRequest "GetPackagePostContent" [t|(PackageName,Slug) -> Maybe Blog.PostContentView|]

packageBlogAPI :: WS.FullAPI '[] _
packageBlogAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPackagePosts
       <:> getPackagePost
       <:> getPackagePostContent
       <:> none



mkRequest "ListPackageVersionTutorials" [t|(PackageName,Version) -> [Tutorial.TutorialView]|]
mkRequest "GetPackageVersionTutorial" [t|(PackageName,Version,Slug) -> Maybe Tutorial.TutorialView|]
mkRequest "GetPackageVersionTutorialContent" [t|(PackageName,Version,Slug) -> Maybe Tutorial.TutorialContentView|]

packageTutorialAPI :: WS.FullAPI '[] _
packageTutorialAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPackageVersionTutorials
       <:> getPackageVersionTutorial
       <:> getPackageVersionTutorialContent
       <:> none



mkRequest "ListPackageVersionModules" [t|(PackageName,Version) -> [Package.ModuleView]|]
mkRequest "ListPackageVersionModulesContent" [t|(PackageName,Version) -> [(Package.ModuleView,Package.ModuleContentView)]|]
mkRequest "GetPackageVersionModule" [t|(PackageName,Version,ModuleName) -> Maybe Package.ModuleView|]
mkRequest "GetPackageVersionModuleContent" [t|(PackageName,Version,ModuleName) -> Maybe Package.ModuleContentView|]

packageModuleAPI :: WS.FullAPI '[] _
packageModuleAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = listPackageVersionModules
       <:> listPackageVersionModulesContent
       <:> getPackageVersionModule
       <:> getPackageVersionModuleContent
       <:> none



api = authorAPI <:+:> 
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
-- up because the module name is the same `Shared`.
mkRequest "Compile" [t|(Txt,Bool) -> Either Txt String|]
mkRequest "ReadModule" [t|String -> Maybe Txt|]

compileAPI :: WS.FullAPI '[] '[Compile,ReadModule]
compileAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = compile <:> readModule <:> none

mkRequest "ResponseTest" [t|() -> ()|]

responseTimeAPI :: WS.FullAPI '[] '[ResponseTest]
responseTimeAPI = WS.api msgs reqs
  where
    msgs = none
    reqs = responseTest <:> none