{-# LANGUAGE CPP #-}
module Components.Titler where

import Pure.Elm

#ifdef __GHCJS__
foreign import javascript unsafe
  "document['title'] = $1" set_title_js :: Txt -> IO ()
#endif

setTitle :: Txt -> IO ()
#ifdef __GHCJS__
setTitle = set_title_js
#else
setTitle = const (return ())
#endif

titler :: Txt -> View
titler = Component $ \self -> def
  { construct = do
    scrollTop
    t <- ask self
    setTitle t
  , receive = \newTitle st -> do
    scrollTop
    setTitle newTitle
    return st
  }

scrollTop =
#ifdef __GHCJS__
  scroll_top_js
foreign import javascript unsafe
  "window.scrollTo({ top: 0, behavior: 'smooth' })" scroll_top_js :: IO ()
#else
  pure ()
#endif
