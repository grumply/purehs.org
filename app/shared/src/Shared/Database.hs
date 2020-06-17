{-# language DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
module Shared.Database where

import qualified Shared.Author as Author
import qualified Shared.Blog as Blog
import qualified Shared.Package as Package
import qualified Shared.Page as Page
import qualified Shared.Tutorial as Tutorial

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Render ()

import GHC.Generics (Generic)

data Database = Database
  { authors :: [Author.AuthorView]
  , blog :: [(Blog.PostView,Blog.PostContentView)]
  , tutorials :: [(Tutorial.TutorialView,Tutorial.TutorialContentView)]
  , packages :: [(Author.AuthorView,[(Package.PackageView,[(Package.VersionView,[(Package.ModuleView,Package.ModuleContentView)],[(Tutorial.TutorialView,Tutorial.TutorialContentView)])])])]
  , pages :: [Page.PageView]
  } deriving (Generic,ToJSON,FromJSON)