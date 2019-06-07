{-# LANGUAGE CPP #-}
module Components.Titler where

import Pure

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
  { construct = ask self >>= setTitle
  , receive = \newTitle st -> setTitle newTitle >> return st
  }
