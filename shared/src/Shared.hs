{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts, PolyKinds, DataKinds, MultiParamTypeClasses, PartialTypeSignatures, DeriveDataTypeable, DuplicateRecordFields #-}
module Shared where

import Pure hiding (Doc)
import Pure.Data.JSON
import Pure.Data.Render
import Pure.WebSocket hiding (api)
import qualified Pure.WebSocket as WS

import GHC.Generics

host = "localhost"
port = 8081

data DocMeta = DocMeta
  { package :: {-# UNPACK #-}!Txt
  , version :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Doc = Doc
  { meta    :: {-# UNPACK #-}!DocMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

data ExampleMeta = ExampleMeta
  { num  :: {-# UNPACK #-}!Txt
  , slug :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Example = Example
  { meta    :: {-# UNPACK #-}!ExampleMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

data PostMeta = PostMeta
  { year  :: {-# UNPACK #-}!Txt
  , month :: {-# UNPACK #-}!Txt
  , day   :: {-# UNPACK #-}!Txt
  , slug  :: {-# UNPACK #-}!Txt
  , title :: {-# UNPACK #-}!Txt -- work around GHCJS toTitle bug
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Post = Post
  { meta    :: {-# UNPACK #-}!PostMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

data TutorialMeta = TutorialMeta
  { number :: {-# UNPACK #-}!Txt
  , slug   :: {-# UNPACK #-}!Txt
  , title  :: {-# UNPACK #-}!Txt -- work around GHCJS toTitle bug
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Tutorial = Tutorial
  { meta    :: {-# UNPACK #-}!TutorialMeta
  , content :: ![View]
  } deriving (Generic,ToJSON,FromJSON)

mkRequest "ReloadMarkdown" [t|() -> ()|]
mkRequest "GetPost" [t|Txt -> Maybe Post|]
mkRequest "GetTutorial" [t|Txt -> Maybe Tutorial|]
mkRequest "GetDoc" [t|(Txt,Txt) -> Maybe Doc|]
mkRequest "GetDocMetas" [t|() -> [DocMeta]|]
mkRequest "GetExamples" [t|() -> [Example]|]
mkRequest "GetPostMetas" [t|() -> [PostMeta]|]
mkRequest "GetTutorialMetas" [t|() -> [TutorialMeta]|]

api = WS.api msgs reqs
  where
    msgs = WS.none
    reqs =
          reloadMarkdown <:>
          getPost <:>
          getTutorial <:>
          getDoc <:>
          getDocMetas <:>
          getExamples <:>
          getPostMetas <:>
          getTutorialMetas <:>
          WS.none
