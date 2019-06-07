module Services.Client where

import System.IO.Unsafe

import Pure.WebSocket as WS

import Imports hiding (Doc)
import Shared

{-# NOINLINE client #-}
client = unsafePerformIO (WS.clientWS "10.0.1.16" 8081)

data ProxyRequest 
  = ReloadMarkdownRequest
  | GetPostRequest Txt
  | GetTutorialRequest Txt
  | GetDocRequest Txt Txt
  | GetDocMetasRequest
  | GetExamplesRequest
  | GetPostMetasRequest
  | GetTutorialMetasRequest

data ProxyResponse
  = ReloadMarkdownResponse
  | GetPostResponse (Maybe Post)
  | GetTutorialResponse (Maybe Tutorial)
  | GetDocResponse (Maybe Doc)
  | GetDocMetasResponse [DocMeta]
  | GetExamplesResponse [Example]
  | GetPostMetasResponse [PostMeta]
  | GetTutorialMetasResponse [TutorialMeta]

data Client m = Client
  { _proxyRequest :: ProxyRequest -> m ProxyResponse
  }
mkCapability ''Client

productionClient :: forall m. MonadIO m => Client m
productionClient = Client {..}
  where
    _proxyRequest :: ProxyRequest -> m ProxyResponse
    _proxyRequest req = liftIO $ do
      let 
        rq ep v f = do
          mv <- newEmptyMVar
          remote Shared.api client ep v (putMVar mv)
          f <$> takeMVar mv
      case req of
        ReloadMarkdownRequest   -> rq reloadMarkdown   ()        (const ReloadMarkdownResponse)
        GetPostRequest slug     -> rq getPost          slug      GetPostResponse
        GetTutorialRequest tut  -> rq getTutorial      tut       GetTutorialResponse
        GetDocRequest pkg ver   -> rq getDoc           (pkg,ver) GetDocResponse
        GetDocMetasRequest      -> rq getDocMetas      ()        GetDocMetasResponse
        GetExamplesRequest      -> rq getExamples      ()        GetExamplesResponse
        GetPostMetasRequest     -> rq getPostMetas     ()        GetPostMetasResponse
        GetTutorialMetasRequest -> rq getTutorialMetas ()        GetTutorialMetasResponse

