{-# LANGUAGE TemplateHaskell, CPP, DeriveAnyClass, ImplicitParams, NoMonomorphismRestriction #-}
module Lib where

import Pure.Data
import Pure.Render
import Pure.View
import Pure.Service
import Pure.WebSocket hiding (api)
import qualified Pure.WebSocket as WS


-- DocMeta

data DocMeta = DocMeta
  { dmAuthor :: {-# UNPACK #-}!Txt
  , dmTitle  :: {-# UNPACK #-}!Txt
  , dmHighlights :: ![(Int,Int)]
  , dmPath   :: {-# UNPACK #-}!(Txt,Txt,Txt)
  } deriving (Generic,ToJSON,FromJSON)

documentNum   = (\(n,_,_) -> n) . dmPath
documentGroup = (\(_,g,_) -> g) . dmPath
documentName  = (\(_,_,n) -> n) . dmPath


-- Doc

data Doc = Doc
  { dMeta :: {-# UNPACK #-}!DocMeta
  , dContent :: ![View '[]]
  } deriving (Generic,ToJSON,FromJSON)


-- ExampleMeta

data ExampleMeta = ExampleMeta
  { emTitle  :: {-# UNPACK #-}!Txt
  , emHighlights :: ![(Int,Int)]
  , emPath   :: {-# UNPACK #-}!(Txt,Txt)
  } deriving (Generic,ToJSON,FromJSON,Eq,Ord)


-- Example

data Example = Example
  { eMeta    :: {-# UNPACK #-}!ExampleMeta
  , eCode    :: ![View '[]]
  , eContent :: ![View '[]]
  } deriving (Generic,ToJSON,FromJSON)


-- Markdown

newtype Markdown = Markdown [View '[]]
  deriving (Generic,ToJSON,FromJSON)


-- PostMeta

data PostMeta = PostMeta
  { pmAuthor :: {-# UNPACK #-}!Txt
  , pmTitle  :: {-# UNPACK #-}!Txt
  , pmHighlights :: ![(Int,Int)]
  , pmPath   :: {-# UNPACK #-}!(Txt,Txt,Txt,Txt)
  } deriving (Generic,ToJSON,FromJSON,Eq,Ord)

postYear  = (\(y,_,_,_) -> y) . pmPath
postMonth = (\(_,m,_,_) -> m) . pmPath
postDay   = (\(_,_,d,_) -> d) . pmPath
postName  = (\(_,_,_,n) -> n) . pmPath


-- Post

data Post = Post
  { pMeta :: {-# UNPACK #-}!PostMeta
  , pContent :: ![View '[]]
  } deriving (Generic,ToJSON,FromJSON)


-- TutorialMeta

data TutorialMeta = TutorialMeta
  { tmAuthor :: {-# UNPACK #-}!Txt
  , tmTitle  :: {-# UNPACK #-}!Txt
  , tmHighlights :: ![(Int,Int)]
  , tmPath   :: {-# UNPACK #-}!(Txt,Txt,Txt,Txt)
  } deriving (Generic,ToJSON,FromJSON)

tutorialNum   = (\(n,_,_,_) -> n) . tmPath
tutorialCh    = (\(_,c,_,_) -> c) . tmPath
tutorialGroup = (\(_,_,g,_) -> g) . tmPath
tutorialSlug  = (\(_,_,_,s) -> s) . tmPath


-- Tutorial

data Tutorial = Tutorial
  { tMeta :: {-# UNPACK #-}!TutorialMeta
  , tContent :: ![View '[]]
  } deriving (Generic,ToJSON,FromJSON)


-- API


mkMessage "ReloadMarkdown" [t|()|]

mkRequest "GetFeaturedTutorial" [t|() -> Maybe Tutorial|]
mkRequest "GetFeaturedPost" [t|() -> Maybe Post|]
mkRequest "GetFeaturedDocumentation" [t|() -> Maybe Doc|]
mkRequest "GetPost" [t|(Txt,Txt,Txt,Txt) -> Maybe Post|]
mkRequest "GetTutorial" [t|(Txt,Txt,Txt,Txt) -> Maybe Tutorial|]
mkRequest "GetDoc" [t|(Txt,Txt,Txt) -> Maybe Doc|]
mkRequest "GetPostMetas" [t|() -> [PostMeta]|]
mkRequest "GetLatestPosts" [t|() -> [Post]|]
mkRequest "GetTutorialMetas" [t|() -> [TutorialMeta]|]
mkRequest "GetDocMetas" [t|() -> [DocMeta]|]
mkRequest "GetExamples" [t|() -> [Example]|]

api = WS.api msgs reqs
  where
    msgs =
          reloadMarkdown <:>
          WS.none
    reqs =
          getFeaturedTutorial <:>
          getFeaturedPost <:>
          getFeaturedDocumentation <:>
          getPost <:>
          getTutorial <:>
          getDoc <:>
          getPostMetas <:>
          getLatestPosts <:>
          getTutorialMetas <:>
          getDocMetas <:>
          getExamples <:>
          WS.none


-- Server Service

purehsorgServer = Service {..}
  where
    key = "purehsorgServer"
    build base = do
#ifdef __GHCJS__
      return (ws ?purehsorgServerIp ?purehsorgServerPort *:* base)
#else
      ws <- websocket unlimited
      return (state ws *:* base)
#endif
    prime = void $ do
#ifdef __GHCJS__
      wsInitialize
      onWSStatus $ \s -> liftIO $ print ("purehsorgServer WSStatus change",s)
#else
      initializeClientWS ?purehsorgServerIp ?purehsorgServerPort "/"
#endif

message = apiMessageWith api purehsorgServer

remote reqPrxy req onSuccess = do
  pr <- withPromise $ \pr -> void $ do
    u <- fresh
    apiRequestWith api purehsorgServer reqPrxy (u,req) $ \done rsp -> lift $ do
      either (const $ fulfill pr Nothing) (fulfill pr . Just) rsp
      done
  attach pr def { success = onSuccess }

