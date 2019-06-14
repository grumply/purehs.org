module View where

import Pure.Elm

import Types
import View.About
import View.Blog
import View.Docs
import View.Home
import View.Tutorials

view model =
  case route model of
    HomeR   -> home model
    AboutR  -> about model
    BlogR _ -> blog model
    DocsR _ -> docs model
    TutsR _ -> tutorials model
