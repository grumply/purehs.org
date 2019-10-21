{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction,
  TemplateHaskell, FlexibleContexts, PolyKinds,
  DataKinds, MultiParamTypeClasses, PartialTypeSignatures,
  DeriveDataTypeable, DuplicateRecordFields
  #-}
module Shared where

import Pure hiding (Doc)
import Pure.Data.JSON
import Pure.Data.Render
import Pure.Data.Try
import Pure.WebSocket hiding (api)
import qualified Pure.WebSocket as WS

import GHC.Generics

import Data.Map as Map

host = "159.65.79.222"
port = 8081

asMap :: Ord k => [(k,v)] -> (Map k v -> Map k v) -> [(k,v)]
asMap kvs f = Map.toList $ f $ Map.fromList kvs

data DocMeta = DocMeta
  { package :: {-# UNPACK #-}!Txt
  , version :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Doc = Doc
  { meta    :: {-# UNPACK #-}!DocMeta
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

data PageMeta = PageMeta
  { slug :: {-# UNPACK #-}!Txt
  } deriving (Eq,Ord,Generic,ToJSON,FromJSON)

data Page = Page
  { meta :: {-# UNPACK #-}!PageMeta
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

data Cache = Cache
  { postMetas :: [PostMeta]
  , docMetas  :: [DocMeta]
  , tutMetas  :: [TutorialMeta]
  , posts     :: [(Txt,Try Post)]
  , docs      :: [((Txt,Txt),Try Doc)]
  , tutorials :: [(Txt,Try Tutorial)]
  , pages     :: [(Txt,Try Page)]
  } deriving (Generic,ToJSON,FromJSON,Default)

mkRequest "GetPost" [t|Txt -> Maybe Post|]
mkRequest "GetTutorial" [t|Txt -> Maybe Tutorial|]
mkRequest "GetDoc" [t|(Txt,Txt) -> Maybe Doc|]
mkRequest "GetPage" [t|Txt -> Maybe Page|]

api = WS.api msgs reqs
  where
    msgs = WS.none
    reqs = getPost <:> getTutorial <:> getDoc <:> getPage <:> WS.none

mkMessage "SetCache" [t|Cache|]

clientApi = WS.api msgs reqs
  where
    msgs = setCache <:> WS.none
    reqs = WS.none
