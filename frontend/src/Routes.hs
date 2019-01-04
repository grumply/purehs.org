module Routes where

import Pure (Txt)
import App
import Shared

data Route
  = NoR

  | HomeR

  | BlogR
  | PostR Txt Txt Txt Txt

  | DocsR
  | DocR Txt Txt Txt

  | TutR
  | TutorialR Txt Txt Txt Txt
