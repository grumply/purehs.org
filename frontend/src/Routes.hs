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
  | PostR Txt

  | DocsR
  | DocR Txt Txt

  | TutsR
  | TutR Txt

  | ExamplesR

-- To highlight active links in the navigation bar, we have to be able to
-- contextualize the route scope; towards that end, this type is a
-- categorization of page context.
data PageType = BlogPage | DocsPage | ExamplesPage | TutsPage
  deriving (Eq,Ord,Enum)

toPageType NoR = Nothing
toPageType HomeR = Nothing
toPageType BlogR = Just BlogPage
toPageType (PostR _) = Just BlogPage
toPageType DocsR = Just DocsPage
toPageType (DocR _ _) = Just DocsPage
toPageType TutsR = Just TutsPage
toPageType (TutR _) = Just TutsPage
toPageType ExamplesR = Just ExamplesPage
