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

  | TutR
  | TutorialR Txt Txt Txt Txt

data PageType = BlogPage | DocsPage | TutsPage
  deriving (Eq,Ord,Enum)

toPageType NoR = Nothing
toPageType HomeR = Nothing
toPageType BlogR = Just BlogPage
toPageType (PostR _ _ _ _) = Just BlogPage
toPageType DocsR = Just DocsPage
toPageType (DocR _ _ _) = Just DocsPage
toPageType TutR = Just TutsPage
toPageType (TutorialR _ _ _ _) = Just TutsPage

