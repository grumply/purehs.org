module Services.Utils where

import Pure.Elm (View())
import Pure.Data.Render
import Pure.Data.Txt as Txt
import Pure.TagSoup

import System.Directory
import System.FilePath
import System.IO

import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import qualified Text.Pandoc.Options as Pandoc

import Text.Read hiding (get,lift)

import Data.List as List

listMarkdownFiles :: FilePath -> IO [FilePath]
listMarkdownFiles cd = List.filter validMarkdown <$> getDirectoryContents cd
  where
    validMarkdown ('.':_) = False
    validMarkdown (takeExtension -> ".md") = True
    validMarkdown _ = False

load :: FilePath -> (Txt -> [View] -> a) -> IO [a]
load sub parse = do
  cd <- (</> "static") <$> getCurrentDirectory
  fs <- listMarkdownFiles (cd </> sub)
  traverse (read cd) fs
  where
    read cd fp@(toTxt . dropExtension -> fn) = do
      cnt <- readFile (cd </> sub </> fp)
      pure $ parse (Txt.replace "^" " " fn) (parseContent $ toTxt cnt)

pattern Dash before after <- (Txt.breakOn "-" -> (before,safeTail -> after))

safeTail t = if Txt.null t then t else Txt.tail t

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
