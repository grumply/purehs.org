{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction, DeriveAnyClass,
             TemplateHaskell, FunctionalDependencies, BangPatterns,
             PackageImports, DeriveGeneric, TypeSynonymInstances,
             FlexibleInstances, DataKinds, TypeApplications, ViewPatterns,
             FlexibleContexts, GADTs, OverloadedStrings, RankNTypes,
             RecordWildCards, PatternSynonyms #-}
module Purehsorg where

import App hiding (none)
import Shared

import Pure hiding (Left,Right,Doc,reverse,none,get,modify)
import qualified Pure.Data.Txt as T
import Pure.Data.JSON hiding (Null,(.=),parseField)
import Pure.WebSocket as WS hiding (respond,Closed)

import qualified Control.Monad.State as St

import Control.Arrow hiding (app)
import Control.Applicative
import Control.Concurrent
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Traversable
import GHC.Generics
import System.Directory
import System.FilePath
import System.IO

import Pure.Data.Render
import Pure.TagSoup

import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import qualified Text.Pandoc.Options as Pandoc

import Text.Read hiding (get,lift)

data State = State
  { docs      :: [Doc]
  , examples  :: [Example]
  , posts     :: [Post]
  , tutorials :: [Tutorial]
  } deriving (Generic,ToJSON,FromJSON)

data Conn = Conn { _connIp :: Txt } deriving (Generic,ToJSON,FromJSON)

type Handler rqTy = (?app :: AppRef State) => ConnRef State Conn -> RequestHandler rqTy


-- main

purehsorg :: IO ()
purehsorg = do
  st <- loadMarkdown
  run host port st impl state conn
  where
    state ip = return (Conn ip)

    conn !as !st = Null


-- API Implementation

impl self = Impl Shared.api msgs reqs
  where
    msgs = none
    reqs =
          handleReloadMarkdown self <:>
          handleGetFeaturedTutorial self <:>
          handleGetFeaturedPost self <:>
          handleGetFeaturedDocumentation self <:>
          handleGetPost self <:>
          handleGetTutorial self <:>
          handleGetDoc self <:>
          handleGetPostMetas self <:>
          handleGetLatestPosts self <:>
          handleGetTutorialMetas self <:>
          handleGetDocMetas self <:>
          handleGetExamples self <:>
          none

loadMarkdown = do
  ds <- loadDocs
  es <- loadExamples
  ps <- loadPosts
  ts <- loadTutorials
  pure (State ds es ps ts)

handleReloadMarkdown :: Handler ReloadMarkdown
handleReloadMarkdown = respond $ do
  st <- liftIO loadMarkdown
  void $ modifyApp (put st)

handleGetFeaturedTutorial :: Handler GetFeaturedTutorial
handleGetFeaturedTutorial = respond Purehsorg.getFeaturedTutorial

handleGetFeaturedPost :: Handler GetFeaturedPost
handleGetFeaturedPost = respond Purehsorg.getFeaturedPost

handleGetFeaturedDocumentation :: Handler GetFeaturedDocumentation
handleGetFeaturedDocumentation = respond Purehsorg.getFeaturedDocumentation

handleGetPost :: Handler GetPost
handleGetPost = respond $ req >>= Purehsorg.getPost

handleGetTutorial :: Handler GetTutorial
handleGetTutorial = respond $ req >>= Purehsorg.getTutorial

handleGetDoc :: Handler GetDoc
handleGetDoc = respond $ req >>= Purehsorg.getDoc

handleGetPostMetas :: Handler GetPostMetas
handleGetPostMetas = respond Purehsorg.getPostMetas

handleGetLatestPosts :: Handler GetLatestPosts
handleGetLatestPosts = respond Purehsorg.getLatestPosts

handleGetTutorialMetas :: Handler GetTutorialMetas
handleGetTutorialMetas = respond Purehsorg.getTutorialMetas

handleGetDocMetas :: Handler GetDocMetas
handleGetDocMetas = respond Purehsorg.getDocMetas

handleGetExamples :: Handler GetExamples
handleGetExamples = respond Purehsorg.getExamples


-- Docs

getDocsFiles :: FilePath -> IO [FilePath]
getDocsFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "docs")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadDocs = do
  cd <- liftIO getCurrentDirectory
  ds <- liftIO $ traverse (readDoc cd) =<< getDocsFiles cd
  pure $ sortBy (compare `on` (dmPath . dMeta)) $ catMaybes ds

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
          dContent = parseContent $ toTxt $ unlines $ drop 5 ls
      in Just Doc {..}

getDoc t = do
  State {..} <- app
  case filter ((t ==) . dmPath . dMeta) docs of
    []    -> send Nothing
    (d:_) -> send (Just d)

getDocMetas = do
  State {..} <- app
  send $ fmap dMeta docs

getFeaturedDocumentation = do
  State {..} <- app
  case docs of
    []    -> send Nothing
    (d:_) -> send (Just d)


-- Examples

getExamplesFiles :: FilePath -> IO [FilePath]
getExamplesFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "examples")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadExamples = do
  cd <- liftIO getCurrentDirectory
  es <- liftIO $ traverse (readExample cd) =<< getExamplesFiles cd
  pure $ sortBy (compare `on` (emPath . eMeta)) (catMaybes es)

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
          eContent = parseContent $ toTxt $ unlines content
          eCode = parseContent $ toTxt $ unlines code
      in Just Example {..}

getExample t = do
  State {..} <- app
  case filter ((t ==) . emPath . eMeta) examples of
    []    -> send Nothing
    (e:_) -> send (Just e)

getExamples = do
  State {..} <- app
  send examples


-- Posts

getPostsFiles :: FilePath -> IO [FilePath]
getPostsFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "posts")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadPosts = do
  cd <- liftIO getCurrentDirectory
  ps <- liftIO $ traverse (readPost cd) =<< getPostsFiles cd
  pure $ sortBy (flip compare `on` (pmPath . pMeta)) (catMaybes ps)

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
          pContent = parseContent $ toTxt $ unlines $ drop 5 ls
      in Just Post {..}

getPost p = do
  State {..} <- app
  case filter ((p ==) . pmPath . pMeta) posts of
    []    -> send Nothing
    (p:_) -> send (Just p)

-- featured post is simply the latest
getFeaturedPost = do
  State {..} <- app
  case posts of
    []    -> send Nothing
    (p:_) -> send (Just p)

getPostMetas = do
  State {..} <- app
  send $ fmap pMeta posts

getLatestPosts = do
  State {..} <- app
  send $ take 10 posts

-- Tutorials

{-
-- Number and chapter control ordering.
-- Num/Ch/Tut/Title
01-01-quickstart-setting-up.md
01-02-quickstart-first-build.md
-}

getTutorialsFiles :: FilePath -> IO [FilePath]
getTutorialsFiles cd = filter validMarkdown <$> getDirectoryContents (cd </> "tutorials")
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

loadTutorials = do
  cd <- liftIO getCurrentDirectory
  ts <- liftIO $ traverse (readTutorial cd) =<< getTutorialsFiles cd
  pure $ sortBy (compare `on` (tmPath . tMeta)) (catMaybes ts)

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
          tContent = parseContent $ toTxt $ unlines $ drop 5 ls
      in Just Tutorial {..}

getTutorial t = do
  State {..} <- app
  case filter ((t ==) . tmPath . tMeta) tutorials of
    []    -> send Nothing
    (t:_) -> send (Just t)

getFeaturedTutorial = do
  State {..} <- app
  case tutorials of
    []    -> send Nothing
    (t:_) -> send (Just t)

getTutorialMetas = do
  State {..} <- app
  send $ map tMeta tutorials


-- Markdown

parseContent :: Txt -> [View]
parseContent cnt =
    let Right !result = Text.Pandoc.Class.runPure $ do
          !md <- readMarkdown Pandoc.def { Pandoc.readerExtensions = Pandoc.pandocExtensions } (T.repack cnt)
          !str <- writeHtml5String Pandoc.def md
          let !view = parseView str
          pure view
    in result

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
