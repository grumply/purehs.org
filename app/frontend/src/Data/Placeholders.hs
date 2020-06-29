module Data.Placeholders where

import Data.Entity

import Shared.Types
import Shared.Package as Package
import Shared.Blog
import Shared.Page
import Shared.Tutorial
import Shared.Author as Author

import Pure.Data.Txt as Txt
import Pure.Elm

import GHC.Exts (IsList(..))

placeholderPackageView :: Package Rendered
placeholderPackageView = Package
  { name = fromTxt "Placeholder"
  , author = fromTxt "Placeholder"
  , latest = fromTxt "0.0.0.0"
  , published = fromTime $ Seconds 0 0
  , license = fromTxt "Copyright"
  , repository = Nothing
  , homepage = Nothing
  , collaborators = fromList []
  , tags = fromList [ "Tag 1", "Tag 2"]
  , description = lorem_ipsum
  }

placeholderPackageContentView :: PackageContent Rendered
placeholderPackageContentView = PackageContent $ fromList
  [ P <||> [ fromTxt lorem_ipsum ]]

placeholderVersionView :: Package.Version Rendered
placeholderVersionView = Package.Version
  { version = fromTxt "0.0.0.0"
  , changes = Changes $ fromList 
      [ P <||> [ fromTxt $ Txt.take 150 lorem_ipsum ] 
      ]
  }

placeholderTutorialView :: Tutorial Rendered
placeholderTutorialView = Tutorial
  { title = fromTxt "Placeholder"
  , subtitle = Nothing
  , slug = "placeholder"
  , series = Nothing
  , episode = Nothing
  , published = fromTime $ Seconds 0 0
  , authors = fromList [fromTxt "Placeholder"]
  , tags = fromList [ "Tag 1", "Tag 2" ]
  , packages = fromList []
  , description = lorem_ipsum
  , excerpt = Excerpt $ fromList
      [ P <||> [ fromTxt $ Txt.take 300 lorem_ipsum ] ]
  }

placeholderTutorialContentView :: TutorialContent Rendered
placeholderTutorialContentView = TutorialContent $ fromList
  [ P <||> [ fromTxt lorem_ipsum ]]

placeholderPostView :: Post Rendered
placeholderPostView = Post
  { title = fromTxt "Placeholder"
  , subtitle = Nothing
  , slug = "placeholder"
  , published = fromTime $ Seconds 0 0   
  , authors = fromList [fromTxt "Placeholder"]
  , tags = fromList [ "Tag 1", "Tag 2" ]
  , description = lorem_ipsum
  , excerpt = Excerpt $ fromList
      [ P <||> [ fromTxt $ Txt.take 300 lorem_ipsum ] ]
  }

placeholderPostContentView :: PostContent Rendered
placeholderPostContentView = PostContent $ fromList
  [ P <||> [ fromTxt lorem_ipsum ]]

placeholderAuthorView :: Author Rendered
placeholderAuthorView = Author.Author
  { name = fromTxt "Placeholder"
  , github = Just $ fromTxt "Placeholder"
  , twitter = Just $ fromTxt "Placeholder"
  , email = Just $ fromTxt "Placeholder"
  , company = Just $ fromTxt "Placeholder"
  , description = lorem_ipsum
  , excerpt = Excerpt $ fromList
      [ P <||> [ fromTxt $ Txt.take 300 lorem_ipsum ] ]
  }

placeholderAuthorContentView :: AuthorContent Rendered
placeholderAuthorContentView = AuthorContent $ fromList
  [ P <||> [ fromTxt lorem_ipsum ]]

placeholderModuleView :: Module Rendered
placeholderModuleView = Module
  { name = fromTxt "Placeholder"
  , description = lorem_ipsum
  , excerpt = Excerpt $ fromList
      [ P <||> [ fromTxt $ Txt.take 300 lorem_ipsum ] ]
  }

placeholderEntities :: [Entity]
placeholderEntities = 
  [ Entity DataType nm v
  , Entity Class__  nm v
  , Entity Function nm v
  , Entity Pattern_ nm v
  ]
  where
    nm = fromTxt "Placeholder"
    v  = EntityView [ P <||> [ fromTxt $ Txt.take 300 lorem_ipsum ] ]

placeholderPageView :: Page
placeholderPageView = Page
  { slug = fromTxt "placeholder"
  , description = lorem_ipsum
  }

placeholderPageContentView :: PageContent Rendered
placeholderPageContentView = PageContent $ fromList
  [ P <||> [ fromTxt lorem_ipsum ] ]

lorem_ipsum :: FromTxt a => a
lorem_ipsum = fromTxt "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin nec risus turpis. Cras quis sodales nunc. Morbi felis est, suscipit quis faucibus sed, porta ut velit. Integer suscipit tellus id laoreet aliquet. Sed condimentum, elit vel consequat semper, tellus neque consectetur nunc, ut vehicula nisi orci ut arcu. Suspendisse ultrices, lectus vitae varius interdum, ex velit laoreet mauris, non placerat lectus elit a nunc. Aliquam sit amet lacinia dolor. Duis auctor quis sem nec tincidunt tellus."
