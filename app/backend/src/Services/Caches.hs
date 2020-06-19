{-# language QuasiQuotes, DuplicateRecordFields #-}
module Services.Caches where

import Pure hiding (name,Left,Right)

import Shared (compileAPI,compile)

import Shared.Author as Author
import Shared.Blog as Blog
import Shared.Package as Package
import Shared.Page as Page
import Shared.Tutorial as Tutorial
import Shared.Types as Types

import Pure.Cached
import Pure.Data.JSON (encodeBS,FromJSON)
import Pure.Data.Time
import Pure.Data.Render ()
import Pure.Data.View (View)
import Pure.Data.Txt as Txt hiding (concat)
import Pure.Data.Txt.Interpolate
import Pure.TagSoup
import Pure.WebSocket as WS

import Control.Concurrent
import Control.Exception (bracket,SomeException(..),handle,throw)
import Data.Char
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Yaml as Yaml

import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import qualified Text.Pandoc.Options as Pandoc

import System.FilePath.Glob as Glob (glob)

import Control.Arrow
import qualified Data.List as List
import Data.Maybe
import Data.Ord
import Data.Function
import Data.Traversable
import System.IO
import System.IO.Unsafe

import GHC.Exts (IsList(..))

epoch :: Time
epoch = Seconds 30 0

{-# NOINLINE authors #-}
authors :: Cached (Map Name (Author Rendered,AuthorContent Rendered))
authors = unsafePerformIO $ 
  forkCache epoch $
    build <$>
      loadAuthors
  where
    build = Map.fromList . fmap process
      where
        nm :: Author Txt -> Name
        nm = name

        process (ay,v) = (nm ay,(fmap parseMarkdown ay,v))

    loadAuthors = catMaybes <$> do
      globs             [i|static/authors/*/|] $ \a ->
        withYamlFile    [i|#{a}author.yaml|]  $ \ay -> do
          m <- markdown [i|#{a}author.md|]
          pure (ay,m)

{-# NOINLINE rawAuthors #-}
rawAuthors :: Cached (Map Name BS.ByteString)
rawAuthors = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached Services.Caches.authors

{-# NOINLINE authorsList #-}
authorsList :: Cached [Author Rendered]
authorsList = unsafePerformIO $ 
  forkCache epoch $
    fmap fst . Map.elems <$> 
      cached Services.Caches.authors

{-# NOINLINE rawAuthorsList #-}
rawAuthorsList :: Cached BS.ByteString
rawAuthorsList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$> 
      cached authorsList

{-# NOINLINE authorsContent #-}
authorsContent :: Cached (Map Name (AuthorContent Rendered))
authorsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap snd <$> 
      cached Services.Caches.authors

{-# NOINLINE rawAuthorsContent #-}
rawAuthorsContent :: Cached (Map Name BS.ByteString)
rawAuthorsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached authorsContent

{-# NOINLINE authorPosts #-}
authorPosts :: Cached (Map Name [Post Rendered])
authorPosts = unsafePerformIO $
  forkCache epoch $
    fmap rebuild (cached posts)
  where
    rebuild :: Map Slug (Post Rendered,PostContent Rendered) -> Map Name [Post Rendered]
    rebuild = reorganize (\_ (pv,_) -> fmap (\a -> (a,pv)) (toList ((Blog.authors :: Post Rendered -> Authors) pv)))

{-# NOINLINE rawAuthorPosts #-}
rawAuthorPosts :: Cached (Map Name BS.ByteString)
rawAuthorPosts = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached authorPosts

{-# NOINLINE authorTutorials #-}
authorTutorials :: Cached (Map Name [Tutorial Rendered])
authorTutorials = unsafePerformIO $
  forkCache epoch $
    fmap rebuild (cached tutorials)
  where
    rebuild :: Map Slug (Tutorial Rendered,TutorialContent Rendered) -> Map Name [Tutorial Rendered]
    rebuild = reorganize (\_ (tv,_) -> fmap (\a -> (a,tv)) (toList ((Tutorial.authors :: Tutorial Rendered -> Authors) tv)))

{-# NOINLINE rawAuthorTutorials #-}
rawAuthorTutorials :: Cached (Map Name BS.ByteString)
rawAuthorTutorials = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached authorTutorials

{-# NOINLINE pages #-}
pages :: Cached (Map Slug (Page Rendered,PageContent Rendered))
pages = unsafePerformIO $ 
  forkCache epoch $
    fmap build loadPages
  where
    build = Map.fromList . fmap process
      where
        slg :: Page Txt -> Slug
        slg = slug

        process (py,pc) = (key,value)
          where
            key = slg py
            value = (fmap parseMarkdown py,pc)

    loadPages = catMaybes <$> do
      globs             [i|static/pages/*/|] $ \d  ->
        withYamlFile    [i|#{d}/page.yaml|]  $ \py -> do
          m <- markdown [i|#{d}/page.md|]
          pure (py,m)

{-# NOINLINE rawPages #-}
rawPages :: Cached (Map Slug BS.ByteString)
rawPages = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached pages

{-# NOINLINE pagesList #-}
pagesList :: Cached [Page Rendered]
pagesList = unsafePerformIO $ 
  forkCache epoch $
    fmap fst . Map.elems <$> 
      cached pages

{-# NOINLINE rawPagesList #-}
rawPagesList :: Cached BS.ByteString
rawPagesList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$> 
      cached pagesList

{-# NOINLINE pagesContent #-}
pagesContent :: Cached (Map Slug (PageContent Rendered))
pagesContent = unsafePerformIO $ 
  forkCache epoch $
    fmap snd <$> 
      cached pages

{-# NOINLINE rawPagesContent #-}
rawPagesContent :: Cached (Map Slug BS.ByteString)
rawPagesContent = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached pagesContent

{-# NOINLINE posts #-}
posts :: Cached (Map Slug (Post Rendered,PostContent Rendered))
posts = unsafePerformIO $ 
  forkCache epoch $
    build <$>
      loadPosts
  where
    build = Map.fromList . fmap process
      where
        slg :: Post Txt -> Slug
        slg = slug

        process (py,m) = (slg py,(fmap parseMarkdown py,m))


    loadPosts = catMaybes <$> do
      globs             [i|static/blog/*/|] $ \d  ->
        withYamlFile    [i|#{d}post.yaml|] $ \py -> do
          m <- markdown [i|#{d}post.md|]
          pure (py,m)

{-# NOINLINE rawPosts #-}
rawPosts :: Cached (Map Slug BS.ByteString)
rawPosts = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached posts

{-# NOINLINE postsList #-}
postsList :: Cached [Post Rendered]
postsList =
  unsafePerformIO $ forkCache epoch $
    fmap fst . Map.elems <$> 
      cached posts

{-# NOINLINE rawPostsList #-}
rawPostsList :: Cached BS.ByteString
rawPostsList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$> 
      cached postsList 

{-# NOINLINE postsContent #-}
postsContent :: Cached (Map Slug (PostContent Rendered))
postsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap snd <$> 
      cached posts

{-# NOINLINE rawPostsContent #-}
rawPostsContent :: Cached (Map Slug BS.ByteString)
rawPostsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached postsContent

{-# NOINLINE tutorials #-}
tutorials :: Cached (Map Slug (Tutorial Rendered,TutorialContent Rendered))
tutorials = unsafePerformIO $ 
  forkCache epoch $
    build <$> 
      loadTutorials
  where
    build = Map.fromList . fmap process
      where
        process (ty,m) = (key,value)
          where
            slg :: Tutorial Txt -> Slug
            slg = slug

            key = slg ty
            value = (fmap parseMarkdown ty,m)

    loadTutorials = catMaybes <$> do
      globs             [i|static/tutorials/*/|] $ \d  ->
        withYamlFile    [i|#{d}tutorial.yaml|] $ \ty -> do
          m <- markdown [i|#{d}tutorial.md|]
          pure (ty,m)

{-# NOINLINE rawTutorials #-}
rawTutorials :: Cached (Map Slug BS.ByteString)
rawTutorials = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached tutorials

{-# NOINLINE rawTutorialsList #-}
rawTutorialsList :: Cached BS.ByteString
rawTutorialsList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS . fmap fst . Map.elems <$> 
      cached tutorials

{-# NOINLINE tutorialsContent #-}
tutorialsContent :: Cached (Map Slug (TutorialContent Rendered))
tutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap snd <$>
      cached tutorials

{-# NOINLINE rawTutorialsContent #-}
rawTutorialsContent :: Cached (Map Slug BS.ByteString)
rawTutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached tutorialsContent

{-# NOINLINE packages #-}
packages :: Cached (Map PackageName (Package Rendered,PackageContent Rendered))
packages = unsafePerformIO $ 
  forkCache epoch $
    fmap build loadPackages
  where
    build = Map.fromList . fmap process
      where
        nm :: Package.Package Txt -> PackageName
        nm = name

        process (py,pm) = (nm py,(fmap parseMarkdown py,pm))

    loadPackages = catMaybes <$> do
      globs          [i|static/packages/*/|] $ \p -> do
        withYamlFile [i|#{p}package.yaml|] $ \yml -> do
          md  <- markdown [i|#{p}package.md|] 
          pure (yml,md)


{-# NOINLINE rawPackages #-}
rawPackages :: Cached (Map PackageName BS.ByteString)
rawPackages = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached Services.Caches.packages

{-# NOINLINE rawPackageContents #-}
rawPackageContents :: Cached (Map PackageName BS.ByteString)
rawPackageContents = unsafePerformIO $
  forkCache epoch $
    fmap (encodeBS . snd) <$>
      cached Services.Caches.packages

{-# NOINLINE packagesList #-}
packagesList :: Cached [Package Rendered]
packagesList = unsafePerformIO $ 
  forkCache epoch $
    (fmap fst . Map.elems) <$> 
      cached Services.Caches.packages

{-# NOINLINE rawPackagesList #-}
rawPackagesList :: Cached BS.ByteString
rawPackagesList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$> 
      cached packagesList

{-# NOINLINE authorPackages #-}
authorPackages :: Cached (Map Name [Package Rendered])
authorPackages = unsafePerformIO $ 
  forkCache epoch $ do
    as :: [Author Rendered] <- cached authorsList
    ps :: Map PackageName (Package Rendered,PackageContent Rendered) <- cached Services.Caches.packages
    aps <- for as $ \a -> 
      let nm = Author.name a
      in pure (nm,fmap fst $ Map.elems $ Map.filter ((nm ==) . author . fst) ps)
    pure (Map.fromList aps)

{-# NOINLINE rawAuthorPackages #-}
rawAuthorPackages :: Cached (Map Name BS.ByteString)
rawAuthorPackages = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached authorPackages

{-# NOINLINE packageVersions #-}
packageVersions :: Cached (Map (PackageName,Types.Version) (Package.Version Rendered))
packageVersions = unsafePerformIO $ 
  forkCache epoch $
    build <$>
      loadVersions
  where
    build = Map.fromList . fmap process
      where
        process (pkgy,py) = (key,value) 
          where
            nm :: Package.Package Txt -> PackageName
            nm = name

            ver :: Package.Version Txt -> Types.Version
            ver = version

            key = (nm pkgy,ver py)
            value = fmap parseMarkdown py

    loadVersions = catMaybes . concat . catMaybes <$> do
      globs                 [i|static/packages/*/|] $ \p  ->
        withYamlFile        [i|#{p}package.yaml|]  $ \pkgy ->
          globs             [i|#{p}versions/*/|]   $ \d ->
            withYamlFile    [i|#{d}version.yaml|]  $ \vy ->
              pure (pkgy,vy)

{-# NOINLINE rawPackageVersions #-}
rawPackageVersions :: Cached (Map (PackageName,Types.Version) BS.ByteString)
rawPackageVersions = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached packageVersions

{-# NOINLINE packageVersionsList #-}
packageVersionsList :: Cached (Map PackageName [Package.Version Rendered])
packageVersionsList = unsafePerformIO $
  forkCache epoch $
    fmap (fmap snd) . pushdown id <$>
      cached packageVersions

{-# NOINLINE rawPackageVersionsList #-}
rawPackageVersionsList :: Cached (Map PackageName BS.ByteString)
rawPackageVersionsList = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached packageVersionsList

{-# NOINLINE packagePosts #-}
packagePosts :: Cached (Map (PackageName,Slug) (Post Rendered,PostContent Rendered))
packagePosts = unsafePerformIO $ 
  forkCache epoch $
    build <$>
      loadPosts
  where
    build = Map.fromList . fmap process
      where
        process (pkgy,psty,m) = (key,value) 
          where
            nm :: Package.Package Txt -> PackageName
            nm = name

            slg :: Post Txt -> Slug
            slg = slug

            key = (nm pkgy,slg psty)
            value = (fmap parseMarkdown psty,m)

    loadPosts = catMaybes . concat . catMaybes <$> do
      globs                 [i|static/packages/*/|] $ \p  ->
        withYamlFile        [i|#{p}package.yaml|]  $ \pkgy ->
          globs             [i|#{p}blog/*/|]       $ \d ->
            withYamlFile    [i|#{d}post.yaml|]     $ \psty -> do
              m <- markdown [i|#{d}post.md|]
              pure (pkgy,psty,m)

{-# NOINLINE rawPackagePosts #-}
rawPackagePosts :: Cached (Map (PackageName,Slug) BS.ByteString)
rawPackagePosts = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached packagePosts

{-# NOINLINE packagePostsList #-}
packagePostsList :: Cached (Map PackageName [Post Rendered])
packagePostsList = unsafePerformIO $ 
  forkCache epoch $
    fmap (fmap (fst . snd)) . pushdown id <$> 
      cached packagePosts

{-# NOINLINE rawPackagePostsList #-}
rawPackagePostsList :: Cached (Map PackageName BS.ByteString)
rawPackagePostsList = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached packagePostsList

{-# NOINLINE packagePostsContent #-}
packagePostsContent :: Cached (Map (PackageName,Slug) (PostContent Rendered))
packagePostsContent = unsafePerformIO $
  forkCache epoch $
    fmap snd <$>
      cached packagePosts

{-# NOINLINE rawPackagePostsContent #-}
rawPackagePostsContent :: Cached (Map (PackageName,Slug) BS.ByteString)
rawPackagePostsContent = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached packagePostsContent

{-# NOINLINE packageTutorials #-}
packageTutorials :: Cached (Map (PackageName,Types.Version,Slug) (Tutorial Rendered,TutorialContent Rendered))
packageTutorials = unsafePerformIO $ 
  forkCache epoch $
    build <$>
      loadPackageTutorials
  where
    build = Map.fromList . fmap process
      where
        process (py,vy,ty,t) = (key,value)
          where
            nm :: Package.Package Txt -> PackageName
            nm = name

            slg :: Tutorial Txt -> Slug
            slg = slug

            ver :: Package.Version Txt -> Types.Version
            ver = version

            key = (nm py,ver vy,slg ty)
            value = (fmap parseMarkdown ty,t)

    loadPackageTutorials = catMaybes . concat . catMaybes . concat . catMaybes <$> do
      globs                     [i|static/packages/*/|] $ \p  ->
        withYamlFile            [i|#{p}package.yaml|]  $ \py ->
          globs                 [i|#{p}versions/*/|]   $ \v  ->
            withYamlFile        [i|#{v}version.yaml|]  $ \vy ->
              globs             [i|#{v}tutorials/*/|]  $ \t  ->
                withYamlFile    [i|#{t}tutorial.yaml|] $ \ty -> do
                  m <- markdown [i|#{t}tutorial.md|]
                  pure (py,vy,ty,m)

{-# NOINLINE rawPackageTutorials #-}
rawPackageTutorials :: Cached (Map (PackageName,Types.Version,Slug) BS.ByteString)
rawPackageTutorials = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached packageTutorials

{-# NOINLINE packageTutorialsList #-}
packageTutorialsList :: Cached (Map (PackageName,Types.Version) [Tutorial Rendered])
packageTutorialsList = unsafePerformIO $ 
  forkCache epoch $
    fmap (fmap (fst . snd)) . pushdown (\(a,b,c) -> ((a,b),c)) <$> 
      cached packageTutorials

{-# NOINLINE rawPackageTutorialsList #-}
rawPackageTutorialsList :: Cached (Map (PackageName,Types.Version) BS.ByteString)
rawPackageTutorialsList = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached packageTutorialsList

{-# NOINLINE packageTutorialsContent #-}
packageTutorialsContent :: Cached (Map (PackageName,Types.Version,Slug) (TutorialContent Rendered))
packageTutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap snd <$>
      cached packageTutorials

{-# NOINLINE rawPackageTutorialsContent #-}
rawPackageTutorialsContent :: Cached (Map (PackageName,Types.Version,Slug) BS.ByteString)
rawPackageTutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached packageTutorialsContent

-- implementation at bottom of file for parsing reasons
{-# NOINLINE modules #-}
modules :: Cached (Map (PackageName,Types.Version,ModuleName) (Module Rendered,ModuleContent Rendered))

{-# NOINLINE rawModules #-}
rawModules :: Cached (Map (PackageName,Types.Version,ModuleName) BS.ByteString)
rawModules = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$> 
      cached modules

{-# NOINLINE modulesList #-}
modulesList :: Cached (Map (PackageName,Types.Version) [Module Rendered])
modulesList = unsafePerformIO $ 
  forkCache epoch $
    fmap (fmap (fst . snd)) . pushdown (\(a,b,c) -> ((a,b),c)) <$> 
      cached modules

{-# NOINLINE rawModulesList #-}
rawModulesList :: Cached (Map (PackageName,Types.Version) BS.ByteString)
rawModulesList = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$> 
      cached modulesList

{-# NOINLINE rawModulesContent #-}
rawModulesContent :: Cached (Map (PackageName,Types.Version,ModuleName) BS.ByteString)
rawModulesContent = unsafePerformIO $
  forkCache epoch $
    fmap (encodeBS . snd) <$>
      cached modules

{-# NOINLINE modulesContentList #-}
modulesContentList :: Cached (Map (PackageName,Types.Version) [(Module Rendered,ModuleContent Rendered)])
modulesContentList = unsafePerformIO $
  forkCache epoch $
    fmap (fmap snd) . pushdown (\(a,b,c) -> ((a,b),c)) <$>
      cached modules

{-# NOINLINE rawModulesContentList #-}
rawModulesContentList :: Cached (Map (PackageName,Types.Version) BS.ByteString)
rawModulesContentList = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$>
      cached modulesContentList

-- Helpers

withYamlFile :: (FromJSON a) => FilePath -> (a -> IO b) -> IO (Maybe b)
withYamlFile fp f = do
  y <- Yaml.decodeFileEither fp
  case y of
    Left e ->
      putStrLn [i|Bad yaml file at #{fp} with error #{show e}|]
        *> hFlush stdout
        *> pure Nothing
    Right a ->
      Just <$> f a

parseMarkdown :: Txt -> Rendered
parseMarkdown cnt =
    let Right result = Text.Pandoc.Class.runPure $ do
          !md <- readMarkdown
                   Pandoc.def
                     { Pandoc.readerExtensions = Pandoc.pandocExtensions }
                   (Txt.repack cnt)
          !str <- writeHtml5String Pandoc.def md
          let !view = parseView str
          pure view
    in Rendered result

markdown :: (FromTxt (f Txt), Functor f, Foldable f) => FilePath -> IO (f Rendered)
markdown fp = do
  f <- T.readFile fp
  Txt.length f `seq` do
    let md = fmap parseMarkdown (fromTxt f)
    traverse_ processExamples md
    pure md 

globs :: FilePath -> (FilePath -> IO a) -> IO [a]
globs p f = do
  xs <- Glob.glob p
  ys <- for xs f
  Prelude.length ys `seq` 
    pure ys

-- interesting helper; not a fan of the (++), but otherwise, interesting.
-- surprisingly good theoretical runtime characteristics
{-# INLINE pushdown #-}
pushdown :: Ord l => (k -> (l,w)) -> Map k v -> Map l [(w,v)]
pushdown f = Map.fromAscListWith (flip (++)) . fmap go . Map.toAscList
  where go (k,v) = let (l,w) = f k in (l,[(w,v)])

{-# INLINE reorganize #-}
reorganize :: Ord l => (k -> v -> [(l,w)]) -> Map k v -> Map l [w]
reorganize f = Map.fromListWith (flip (++)) . List.concatMap go . Map.toAscList
  where go (k,v) = fmap (fmap (\x -> [x])) (f k v)

-- type and pragma above for parsing reasons
-- modules :: Cached (Map (PackageName,Types.Version,ModuleName) (Module Rendered,ModuleContent Rendered))
modules =
  unsafePerformIO $ forkCache epoch $
    fmap build loadModules
  where
    build = Map.fromList . fmap process
      where
        process (py,vy,my,md) = (key,value) 
          where
            nm :: Package.Package Txt -> PackageName
            nm = name

            mdl :: Module Txt -> ModuleName
            mdl = name

            ver :: Package.Version Txt -> Types.Version
            ver = version

            key   = (nm py,ver vy,mdl my)
            value = (fmap parseMarkdown my,md)

    loadModules = catMaybes . concat . catMaybes . concat . catMaybes <$> do
      globs                     [i|static/packages/*/|] $ \p  ->
        withYamlFile            [i|#{p}package.yaml|]  $ \py ->
          globs                 [i|#{p}versions/*/|]   $ \v  ->
            withYamlFile        [i|#{v}version.yaml|]  $ \vy ->
              globs             [i|#{v}modules/*/|]    $ \m  ->
                -- syntax parser don't like `module` inside quasi-quotes
                withYamlFile    [i|#{m}module.yaml|]  $ \my -> do
                  md <- markdown [i|#{m}module.md|]
                  pure (py,vy,my,md)

{-# NOINLINE tryWS #-}
tryWS :: WebSocket
tryWS = unsafePerformIO $ clientWS "204.48.20.19" 8080

processExamples :: Rendered -> IO ()
processExamples (Rendered md) = traverse_ example md
  where
    example :: View -> IO ()
    example (Children (texts -> t) (Attributes as Pre)) 
      | Just _ <- List.lookup "data-try" as = void $
        remote compileAPI tryWS compile (Txt.dropWhile isSpace t,True) print
    example v = traverse_ example (getChildren v)

texts :: [View] -> Txt
texts = Txt.dropWhile isSpace . List.foldl' append ""
  where
    append t (TextView _ a) = t <> "\n" <> (Txt.dropWhileEnd isSpace a)
    append t _ = t
