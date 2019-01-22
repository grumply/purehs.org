{-# LANGUAGE ImplicitParams, DeriveAnyClass, BangPatterns, DeriveGeneric,
             ViewPatterns, OverloadedStrings, RecordWildCards,
             PatternSynonyms, TypeApplications, DuplicateRecordFields,
             RankNTypes, NoMonomorphismRestriction, FlexibleContexts
  #-}
module Purehsorg where

import App hiding (none)
import Shared

import Pure hiding (Left,Right,Doc,reverse,none,get,modify)
import qualified Pure.Data.Txt as Txt
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
          handleGetPost self <:>
          handleGetTutorial self <:>
          handleGetDoc self <:>
          handleGetPostMetas self <:>
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
handleReloadMarkdown conn = responding $ do
  st <- liftIO loadMarkdown
  void $ modifyApp (put st)

handleGetPost :: Handler GetPost
handleGetPost conn = responding $ acquire >>= Purehsorg.getPost

handleGetTutorial :: Handler GetTutorial
handleGetTutorial conn = responding $ acquire >>= Purehsorg.getTutorial

handleGetDoc :: Handler GetDoc
handleGetDoc conn = responding $ acquire >>= Purehsorg.getDoc

handleGetPostMetas :: Handler GetPostMetas
handleGetPostMetas conn = responding Purehsorg.getPostMetas

handleGetTutorialMetas :: Handler GetTutorialMetas
handleGetTutorialMetas conn = responding Purehsorg.getTutorialMetas

handleGetDocMetas :: Handler GetDocMetas
handleGetDocMetas conn = responding Purehsorg.getDocMetas

handleGetExamples :: Handler GetExamples
handleGetExamples conn = responding Purehsorg.getExamples


-- Utilities

listMarkdownFiles :: FilePath -> IO [FilePath]
listMarkdownFiles cd = filter validMarkdown <$> getDirectoryContents cd
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

load :: FilePath -> (Txt -> [View] -> a) -> IO [a]
load sub parse = do
  cd <- getCurrentDirectory
  fs <- listMarkdownFiles (cd </> sub)
  traverse (read cd) fs
  where
    read cd fp@(toTxt . dropExtension -> fn) = do
      cnt <- readFile (cd </> sub </> fp)
      pure $ parse (Txt.replace "^" " " fn) (parseContent $ toTxt cnt)

safeTail t = if Txt.null t then t else Txt.tail t

pattern Dash before after <- (Txt.breakOn "-" -> (before,safeTail -> after))

parseContent :: Txt -> [View]
parseContent cnt =
    let Right !result = Text.Pandoc.Class.runPure $ do
          !md <- readMarkdown
                   Pandoc.def
                     { Pandoc.readerExtensions = Pandoc.pandocExtensions }
                   (Txt.repack cnt)
          !str <- writeHtml5String Pandoc.def md
          let !view = parseView str
          pure view
    in result

sendMaybeFromList [] = reply Nothing
sendMaybeFromList (x : _) = reply (Just x)


-- Docs

dMeta Doc {..} = meta

loadDocs = fmap (sortOn dMeta) $ load "docs" $ \fn ->
  let (Txt.reverse -> version) `Dash` (Txt.reverse -> package) = Txt.reverse fn
  in Doc DocMeta {..}

getDoc (uncurry DocMeta -> m) = do
  State {..} <- app
  sendMaybeFromList (filter ((m ==) . dMeta) docs)

getDocMetas = do
  State {..} <- app
  reply (fmap dMeta docs)


-- Examples

eMeta Example {..} = meta

loadExamples = fmap (sortOn eMeta) $ load "examples" $ \fn ->
  let num `Dash` slug = fn
  in Example ExampleMeta {..}

getExamples = do
  State {..} <- app
  reply examples


-- Posts

pMeta Post {..} = meta
pmSlug PostMeta {..} = slug

loadPosts = fmap (reverse . sortOn pMeta) $ load "posts" $ \fn ->
  let year `Dash` (month `Dash` (day `Dash` slug_)) = fn
      slug = Txt.replace "_" "-" slug_
      title = Txt.toTitle . Txt.replace "_" "-"  . Txt.replace "-" " " $ slug_
  in Post PostMeta {..}

getPost s = do
  State {..} <- app
  sendMaybeFromList (filter ((s ==) . pmSlug . pMeta) posts)

getPostMetas = do
  State {..} <- app
  reply (fmap pMeta posts)


-- Tutorials

tMeta Tutorial {..} = meta
tmSlug TutorialMeta {..} = slug

loadTutorials = fmap (sortOn tMeta) $ load "tutorials" $ \fn ->
  let number `Dash` slug = fn
      title = Txt.toTitle slug
  in Tutorial TutorialMeta {..}

getTutorial s = do
  State {..} <- app
  sendMaybeFromList (filter ((s ==) . tmSlug . tMeta) tutorials)

getTutorialMetas = do
  State {..} <- app
  reply (fmap tMeta tutorials)
