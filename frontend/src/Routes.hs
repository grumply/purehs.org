module Routes where

import Pure (Txt)
import Pure.Data.JSON
import App
import Shared

import GHC.Generics

data Route
  = NoR

  | HomeR

  | BlogR
  | PostR Txt Txt Txt Txt

  | DocsR
  | DocR Txt Txt Txt

  | TutsR
  | TutorialR Txt Txt Txt Txt

  | ExamplesR
  | ExampleR Txt

-- To highlight active links in the navigation bar, we have to be able to
-- contextualize the route scope; towards that end, this type is a
-- categorization of page context.
data PageType = BlogPage | DocsPage | ExamplesPage | TutsPage
  deriving (Eq,Ord,Enum)

toPageType NoR = Nothing

toPageType HomeR = Nothing

toPageType BlogR = Just BlogPage
toPageType (PostR _ _ _ _) = Just BlogPage

toPageType DocsR = Just DocsPage
toPageType (DocR _ _ _) = Just DocsPage

toPageType TutsR = Just TutsPage
toPageType (TutorialR _ _ _ _) = Just TutsPage

toPageType ExamplesR = Just ExamplesPage
toPageType (ExampleR _) = Just ExamplesPage
