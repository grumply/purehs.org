{-# LANGUAGE PatternSynonyms, TypeOperators #-}
module Main where

import Pure.Server hiding (parseField)
import Pure.Service hiding (parseField)
import Pure.Connection hiding (parseField)

import Lib

import Control.Arrow
import Data.Maybe
import Data.Char
import Data.List
import Data.Function
import Data.Maybe

import System.IO
import System.Directory
import System.FilePath

import Pure.TagSoup
import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Extensions (githubMarkdownExtensions)
import qualified Text.Pandoc.Options as Options

import Options.Generic hiding (parseField)

import qualified Pure.Data.Txt as T

import Text.Read (readMaybe)


-- Args

data Args = Args
  { addr :: String <?> "ip:port pair for the PurehsorgServer server"
  {- END ARGS -}
  } deriving (Generic)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

getArgs :: IO Args
getArgs = getRecord "PurehsorgServer"


-- main

main :: IO ()
main = do
  args <- getArgs
  let ip   = "10.0.1.21" -- parseIp   $ unHelpful $ addr args
      port = parsePort $ unHelpful $ addr args
  run (Main.app args ip port)


-- Server

app args ip port = Server {..}
  where
    key   = "PurehsorgServer"
    build = return
    prime = return ()
    connection = newConnection args


-- Connection

newConnection args _ = Connection {..}
  where
    build = return
    prime = do
      enact $
        let
        in impl
      setExhaustLimit maxBound
      return ()


-- API Implementation

impl = Impl Lib.api msgs reqs
  where
    msgs =
          handleReloadMarkdown <:>
          none
    reqs =
          handleGetFeaturedTutorial <:>
          handleGetFeaturedPost <:>
          handleGetFeaturedDocumentation <:>
          handleGetPost <:>
          handleGetTutorial <:>
          handleGetDoc <:>
          handleGetPostMetas <:>
          handleGetLatestPosts <:>
          handleGetTutorialMetas <:>
          handleGetDocMetas <:>
          handleGetExamples <:>
          none

handleReloadMarkdown = accepts Lib.reloadMarkdown $ \_ q ->
  lift_ $ for q $ \req -> void $ do
    with _Example loadExamples
    with _Doc loadDocs
    with _Post loadPosts
    with _Tutorial loadTutorials

proxyRequest ep service f = responds ep $ \_ q ->
  lift_ $ for q $ \(rsp,(_,req)) -> do
    pr <- with service (f req)
    attach pr def { success = void . rsp . Right }

handleGetFeaturedTutorial      = proxyRequest Lib.getFeaturedTutorial _Tutorial Main.getFeaturedTutorial
handleGetFeaturedPost          = proxyRequest Lib.getFeaturedPost _Post Main.getFeaturedPost
handleGetFeaturedDocumentation = proxyRequest Lib.getFeaturedDocumentation _Doc Main.getFeaturedDocumentation
handleGetPost                  = proxyRequest Lib.getPost _Post Main.getPost
handleGetTutorial              = proxyRequest Lib.getTutorial _Tutorial Main.getTutorial
handleGetDoc                   = proxyRequest Lib.getDoc _Doc Main.getDoc
handleGetPostMetas             = proxyRequest Lib.getPostMetas _Post Main.getPostMetas
handleGetLatestPosts           = proxyRequest Lib.getLatestPosts _Post Main.getLatestPosts
handleGetTutorialMetas         = proxyRequest Lib.getTutorialMetas _Tutorial Main.getTutorialMetas
handleGetDocMetas              = proxyRequest Lib.getDocMetas _Doc Main.getDocMetas
handleGetExamples              = proxyRequest Lib.getExamples _Example Main.getExamples


-- Doc Service

data DocState = DS
  { dsDocs :: [Doc]
  }

_Doc = Service {..}
  where
    key = "Doc"
    build base = return (state (DS []) *:* base)
    prime = do
      loadDocs

getDocsFiles :: FilePath -> IO [FilePath]
getDocsFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "docs")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadDocs = do
  cd <- liftIO getCurrentDirectory
  ds <- liftIO $ traverse (readDoc cd) =<< getDocsFiles cd
  void $ modify $ \DS {..} -> (DS { dsDocs = sortBy (flip compare `on` (dmPath . dMeta)) $ catMaybes ds, .. },())

safeTail t = if T.null t then t else T.tail t

pattern Dash before after <- (T.breakOn "-" -> (before,safeTail -> after))

readDoc :: FilePath -> FilePath -> IO (Maybe Doc)
readDoc cd fp = do
  cnt <- readFile (cd </> "docs" </> fp)
  return $ parseDoc fp cnt
  where
    parsePath :: FilePath -> (Txt,Txt,Txt)
    parsePath (toTxt . dropExtension -> num `Dash` (group `Dash` name)) = (num,group,name)

    parseDoc :: FilePath -> String -> Maybe Doc
    parseDoc (parsePath -> dmPath) cnt@(lines -> ls) =
      let (dmAuthor,(dmTitle,dmHighlights)) = (parseAuthor &&& parseTitle &&& parseHighlights) $ take 3 $ tail ls
          dMeta = DocMeta {..}
          dContent = parseMarkdown $ toTxt $ unlines $ drop 5 ls
      in Just Doc {..}

getDoc t = do
  DS {..} <- get
  case filter ((t ==) . dmPath . dMeta) dsDocs of
    []    -> return Nothing
    (d:_) -> return (Just d)

getDocMetas req = do
  DS {..} <- get
  return $ map dMeta dsDocs

getFeaturedDocumentation _ = do
  DS {..} <- get
  case dsDocs of
    [] -> return Nothing
    (d:_) -> return (Just d)


-- Example Service

data ExampleState = ES
  { esExamples :: [Example]
  }

_Example = Service {..}
  where
    key = "Example"
    build base = return (state (ES []) *:* base)
    prime = loadExamples

getExamplesFiles :: FilePath -> IO [FilePath]
getExamplesFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "examples")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadExamples = do
  cd <- liftIO getCurrentDirectory
  es <- liftIO $ traverse (readExample cd) =<< getExamplesFiles cd
  void $ modify $ \ES {..} -> (ES { esExamples = sortBy (flip compare `on` (emPath . eMeta)) (catMaybes es), .. },())

readExample :: FilePath -> FilePath -> IO (Maybe Example)
readExample cd fp = do
  cnt <- readFile (cd </> "examples" </> fp)
  return $ parseExample fp cnt
  where
    parsePath :: FilePath -> (Txt,Txt)
    parsePath (toTxt -> num `Dash` file) = (num,file)

    parseExample :: FilePath -> String -> Maybe Example
    parseExample (parsePath -> emPath) cnt@(lines -> ls) =
      let (emTitle,emHighlights) = (parseTitle &&& parseHighlights) $ take 2 $ tail ls
          eMeta = ExampleMeta {..}
          (content,_:code) = span (/= "# Code") (drop 4 ls)
          eContent = parseMarkdown $ toTxt $ unlines content
          eCode = parseMarkdown $ toTxt $ unlines code
      in Just Example {..}

getExample t = do
  ES {..} <- get
  case filter ((t ==) . emPath . eMeta) esExamples of
    []    -> return Nothing
    (e:_) -> return (Just e)

getExamples _ = do
  ES {..} <- get
  return esExamples


-- Markdown Service

parseMarkdown cnt =
  let mv = fmap parseView . runPure $ do
             md <- readMarkdown Options.def { Options.readerExtensions = githubMarkdownExtensions } cnt
             writeHtml5String Options.def md
  in case mv of
       Left  _   -> []
       Right !vs -> vs

parseAuthor :: [String] -> Txt
parseAuthor = parseField "author:"

parseTitle :: [String] -> Txt
parseTitle = parseField "title:"

parseHighlights :: [String] -> [(Int,Int)]
parseHighlights ls =
  let hs = parseField "highlights:" ls
  in fromMaybe [] $ readMaybe (fromTxt hs)

parseField :: String -> [String] -> Txt
parseField field ls =
  case filter (field `isInfixOf`) ls of
    [] -> ""
    (f:_) -> toTxt $ trim $ tail $ dropWhile (/= ':') f
  where
    trim = unpad . unpad
    unpad = dropWhile isSpace . reverse


-- Post Service

data PostState = PS
  { psPosts :: [Post]
  }

_Post = Service {..}
  where
    key = "Post"
    build base = return (state (PS []) *:* base)
    prime = loadPosts

getPostsFiles :: FilePath -> IO [FilePath]
getPostsFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "posts")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadPosts = do
  cd <- liftIO getCurrentDirectory
  ps <- liftIO $ traverse (readPost cd) =<< getPostsFiles cd
  void $ modify $ \PS {..} -> (PS { psPosts = sortBy (flip compare `on` (pmPath . pMeta)) $ catMaybes ps, .. },())

readPost :: FilePath -> FilePath -> IO (Maybe Post)
readPost cd fp = do
  cnt <- readFile (cd </> "posts" </> fp)
  return $ parsePost fp cnt
  where
    parsePath :: FilePath -> (Txt,Txt,Txt,Txt)
    parsePath (toTxt . dropExtension -> year `Dash` (month `Dash` (day `Dash` title))) = (year,month,day,title)

    parsePost :: FilePath -> String -> Maybe Post
    parsePost (parsePath -> pmPath) cnt@(lines -> ls) =
      let (pmAuthor,(pmTitle,pmHighlights)) = (parseAuthor &&& parseTitle &&& parseHighlights) $ take 3 $ tail ls
          pMeta = PostMeta {..}
          pContent = parseMarkdown $ toTxt $ unlines $ drop 5 ls
      in Just Post {..}

getPost p = do
  PS {..} <- get
  case filter ((p ==) . pmPath . pMeta) psPosts of
    []    -> return Nothing
    (p:_) -> return (Just p)

-- featured post is simply the latest
getFeaturedPost _ = do
  PS {..} <- get
  case psPosts of
    []    -> return Nothing
    (p:_) -> return (Just p)

getPostMetas _ = do
  PS {..} <- get
  return $ map pMeta psPosts

getLatestPosts _ = do
  PS {..} <- get
  return $ take 10 psPosts

-- Tutorial Service

{-
-- Num/Ch/Tut/Title
01-01-quickstart-setting-up.md
01-02-quickstart-first-build.md
-}

data TutorialState = TS
  { tsTutorials :: [Tutorial]
  }

_Tutorial = Service {..}
  where
    key = "Tutorial"
    build base = return (state (TS []) *:* base)
    prime = loadTutorials

getTutorialsFiles :: FilePath -> IO [FilePath]
getTutorialsFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "tutorials")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadTutorials = do
  cd <- liftIO getCurrentDirectory
  ts <- liftIO $ traverse (readTutorial cd) =<< getTutorialsFiles cd
  void $ modify $ \TS {..} -> (TS { tsTutorials = sortBy (compare `on` (tmPath . tMeta)) $ catMaybes ts, .. },())

readTutorial :: FilePath -> FilePath -> IO (Maybe Tutorial)
readTutorial cd fp = do
  cnt <- readFile (cd </> "tutorials" </> fp)
  return $ parseTutorial fp cnt
  where
    parsePath :: FilePath -> (Txt,Txt,Txt,Txt)
    parsePath (toTxt . dropExtension -> group `Dash` (chapter `Dash` (name `Dash` title))) = (group,chapter,name,title)

    parseTutorial :: FilePath -> String -> Maybe Tutorial
    parseTutorial (parsePath -> tmPath) cnt@(lines -> ls) =
      let (tmAuthor,(tmTitle,tmHighlights)) = (parseAuthor &&& parseTitle &&& parseHighlights) $ take 3 $ tail ls
          tMeta = TutorialMeta {..}
          tContent = parseMarkdown $ toTxt $ unlines $ drop 5 ls
      in Just Tutorial {..}


getTutorial t = do
  TS {..} <- get
  case filter ((t ==) . tmPath . tMeta) tsTutorials of
    []    -> return Nothing
    (t:_) -> return (Just t)

getFeaturedTutorial _ = do
  TS {..} <- get
  case tsTutorials of
    []    -> return Nothing
    (t:_) -> return (Just t)

getTutorialMetas req = do
  TS {..} <- get
  return $ map tMeta tsTutorials
