module Router where

import Pure.Elm
import Types

router :: Routing Msg ()
router = do
 
  path "/blog/:slug" $ do
    s <- "slug"
    dispatch $ Route $ BlogR $ Just s

  path "/doc/:pkg/:ver" $ do
    p <- "pkg"
    v <- "ver"
    dispatch $ Route $ DocsR $ Just (p,v)

  path "/tut/:slug" $ do
    s <- "slug"
    dispatch $ Route $ TutsR $ Just s

  path "/blog" $ dispatch $ Route $ BlogR Nothing

  path "/docs" $ dispatch $ Route $ DocsR Nothing

  path "/tuts" $ dispatch $ Route $ TutsR Nothing

  path "/about" $ dispatch $ Route AboutR

  dispatch $ Route HomeR


