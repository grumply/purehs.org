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

import Pure.Cached ( cached, forkCache, Cached )
import Pure.Data.JSON (encodeBS,FromJSON)
import Pure.Data.Render ()
import Pure.Data.View (View)
import Pure.Data.Txt as Txt
    ( dropWhile, dropWhileEnd, length, repack )
import Pure.Data.Txt.Interpolate ( i )
import Pure.TagSoup ( parseView )
import Pure.WebSocket as WS ( request, clientWS, WebSocket )

import Data.Char ( isSpace )
import qualified Data.ByteString as BS ( length, readFile )
import qualified Data.ByteString.Lazy as BSL ( ByteString )
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Yaml as Yaml ( decodeEither' )

import Text.Pandoc.Class ( runPure )
import Text.Pandoc.Readers.Markdown ( readMarkdown )
import Text.Pandoc.Writers.HTML ( writeHtml5String )
import qualified Text.Pandoc.Options as Pandoc

import System.FilePath.Glob as Glob ( glob )

import qualified Data.List as List
import Data.Maybe ( catMaybes )
import System.IO ( stdout, hFlush )
import System.IO.Unsafe ( unsafePerformIO )

import GHC.Exts (IsList(..))

epoch :: Time
epoch = Minutes 30 0

{-# NOINLINE authors #-}
authors :: Cached (Map Name (Author Rendered,AuthorContent Rendered))
authors = unsafePerformIO $ 
  forkCache epoch $
    build <$!>
      loadAuthors
  where
    build = Map.fromList . fmap process
      where
        nm :: Author Txt -> Name
        nm = name

        process (ay,v) = (nm ay,(fmap parseMarkdown ay,v))

    loadAuthors = catMaybes <$!> do
      globs             [i|static/authors/*/|] $ \a ->
        withYamlFile    [i|#{a}author.yaml|]  $ \ay -> do
          m <- markdown [i|#{a}author.md|]
          pure (ay,m)

{-# NOINLINE rawAuthors #-}
rawAuthors :: Cached (Map Name BSL.ByteString)
rawAuthors = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached Services.Caches.authors

{-# NOINLINE authorsList #-}
authorsList :: Cached [Author Rendered]
authorsList = unsafePerformIO $ 
  forkCache epoch $
    fmap fst . Map.elems <$!> 
      cached Services.Caches.authors

{-# NOINLINE rawAuthorsList #-}
rawAuthorsList :: Cached BSL.ByteString
rawAuthorsList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$!> 
      cached authorsList

{-# NOINLINE authorsContent #-}
authorsContent :: Cached (Map Name (AuthorContent Rendered))
authorsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap snd <$!> 
      cached Services.Caches.authors

{-# NOINLINE rawAuthorsContent #-}
rawAuthorsContent :: Cached (Map Name BSL.ByteString)
rawAuthorsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached authorsContent

{-# NOINLINE authorPosts #-}
authorPosts :: Cached (Map Name [Post Rendered])
authorPosts = unsafePerformIO $
  forkCache epoch $
    fmap rebuild (cached posts)
  where
    rebuild :: Map Slug (Post Rendered,PostContent Rendered) -> Map Name [Post Rendered]
    rebuild = splits (\_ (pv,_) -> fmap (\a -> (a,pv)) (toList ((Blog.authors :: Post Rendered -> Authors) pv)))

{-# NOINLINE rawAuthorPosts #-}
rawAuthorPosts :: Cached (Map Name BSL.ByteString)
rawAuthorPosts = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached authorPosts

{-# NOINLINE authorTutorials #-}
authorTutorials :: Cached (Map Name [Tutorial Rendered])
authorTutorials = unsafePerformIO $
  forkCache epoch $
    fmap rebuild (cached tutorials)
  where
    rebuild :: Map Slug (Tutorial Rendered,TutorialContent Rendered) -> Map Name [Tutorial Rendered]
    rebuild = splits (\_ (tv,_) -> fmap (\a -> (a,tv)) (toList ((Tutorial.authors :: Tutorial Rendered -> Authors) tv)))

{-# NOINLINE rawAuthorTutorials #-}
rawAuthorTutorials :: Cached (Map Name BSL.ByteString)
rawAuthorTutorials = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached authorTutorials

{-# NOINLINE pages #-}
pages :: Cached (Map Slug (Page,PageContent Rendered))
pages = unsafePerformIO $ 
  forkCache epoch $
    fmap build loadPages
  where
    build = Map.fromList . fmap process
      where
        slg :: Page -> Slug
        slg = slug

        process (py,pc) = (key,value)
          where
            key = slg py
            value = (py,pc)

    loadPages = catMaybes <$!> do
      globs             [i|static/pages/*/|] $ \d  ->
        withYamlFile    [i|#{d}/page.yaml|]  $ \py -> do
          m <- markdown [i|#{d}/page.md|]
          pure (py,m)

{-# NOINLINE rawPages #-}
rawPages :: Cached (Map Slug BSL.ByteString)
rawPages = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached pages

{-# NOINLINE pagesList #-}
pagesList :: Cached [Page]
pagesList = unsafePerformIO $ 
  forkCache epoch $
    fmap fst . Map.elems <$!> 
      cached pages

{-# NOINLINE rawPagesList #-}
rawPagesList :: Cached BSL.ByteString
rawPagesList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$!> 
      cached pagesList

{-# NOINLINE pagesContent #-}
pagesContent :: Cached (Map Slug (PageContent Rendered))
pagesContent = unsafePerformIO $ 
  forkCache epoch $
    fmap snd <$!> 
      cached pages

{-# NOINLINE rawPagesContent #-}
rawPagesContent :: Cached (Map Slug BSL.ByteString)
rawPagesContent = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached pagesContent

{-# NOINLINE posts #-}
posts :: Cached (Map Slug (Post Rendered,PostContent Rendered))
posts = unsafePerformIO $ 
  forkCache epoch $
    build <$!>
      loadPosts
  where
    build = Map.fromList . fmap process
      where
        slg :: Post Txt -> Slug
        slg = slug

        process (py,m) = (slg py,(fmap parseMarkdown py,m))


    loadPosts = catMaybes <$!> do
      globs             [i|static/blog/*/|] $ \d  ->
        withYamlFile    [i|#{d}post.yaml|] $ \py -> do
          m <- markdown [i|#{d}post.md|]
          pure (py,m)

{-# NOINLINE rawPosts #-}
rawPosts :: Cached (Map Slug BSL.ByteString)
rawPosts = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached posts

{-# NOINLINE postsList #-}
postsList :: Cached [Post Rendered]
postsList =
  unsafePerformIO $ forkCache epoch $
    fmap fst . Map.elems <$!> 
      cached posts

{-# NOINLINE rawPostsList #-}
rawPostsList :: Cached BSL.ByteString
rawPostsList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$!> 
      cached postsList 

{-# NOINLINE postsContent #-}
postsContent :: Cached (Map Slug (PostContent Rendered))
postsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap snd <$!> 
      cached posts

{-# NOINLINE rawPostsContent #-}
rawPostsContent :: Cached (Map Slug BSL.ByteString)
rawPostsContent = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached postsContent

{-# NOINLINE tutorials #-}
tutorials :: Cached (Map Slug (Tutorial Rendered,TutorialContent Rendered))
tutorials = unsafePerformIO $ 
  forkCache epoch $
    build <$!> 
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

    loadTutorials = catMaybes <$!> do
      globs             [i|static/tutorials/*/|] $ \d  ->
        withYamlFile    [i|#{d}tutorial.yaml|] $ \ty -> do
          m <- markdown [i|#{d}tutorial.md|]
          pure (ty,m)

{-# NOINLINE rawTutorials #-}
rawTutorials :: Cached (Map Slug BSL.ByteString)
rawTutorials = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached tutorials

{-# NOINLINE rawTutorialsList #-}
rawTutorialsList :: Cached BSL.ByteString
rawTutorialsList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS . fmap fst . Map.elems <$!> 
      cached tutorials

{-# NOINLINE tutorialsContent #-}
tutorialsContent :: Cached (Map Slug (TutorialContent Rendered))
tutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap snd <$!>
      cached tutorials

{-# NOINLINE rawTutorialsContent #-}
rawTutorialsContent :: Cached (Map Slug BSL.ByteString)
rawTutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached tutorialsContent

{-# NOINLINE packages #-}
packages :: Cached (Map PackageName (Package,PackageContent Rendered))
packages = unsafePerformIO $ 
  forkCache epoch $
    fmap build loadPackages
  where
    build = Map.fromList . fmap process
      where
        nm :: Package.Package -> PackageName
        nm = name

        process (py,pm) = (nm py,(py,pm))

    loadPackages = catMaybes <$!> do
      globs          [i|static/packages/*/|] $ \p -> do
        withYamlFile [i|#{p}package.yaml|] $ \yml -> do
          md  <- markdown [i|#{p}package.md|] 
          pure (yml,md)

{-# NOINLINE rawPackages #-}
rawPackages :: Cached (Map PackageName BSL.ByteString)
rawPackages = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached Services.Caches.packages

{-# NOINLINE rawPackageContents #-}
rawPackageContents :: Cached (Map PackageName BSL.ByteString)
rawPackageContents = unsafePerformIO $
  forkCache epoch $
    fmap (encodeBS . snd) <$!>
      cached Services.Caches.packages

{-# NOINLINE packagesList #-}
packagesList :: Cached [Package]
packagesList = unsafePerformIO $ 
  forkCache epoch $
    (fmap fst . Map.elems) <$!> 
      cached Services.Caches.packages

{-# NOINLINE rawPackagesList #-}
rawPackagesList :: Cached BSL.ByteString
rawPackagesList = unsafePerformIO $ 
  forkCache epoch $
    encodeBS <$!> 
      cached packagesList

{-# NOINLINE authorPackages #-}
authorPackages :: Cached (Map Name [Package])
authorPackages = unsafePerformIO $ 
  forkCache epoch $ do
    as :: [Author Rendered] <- cached authorsList
    ps :: Map PackageName (Package,PackageContent Rendered) <- cached Services.Caches.packages
    aps <- for as $ \a -> 
      let nm = Author.name a
      in pure (nm,fmap fst $ Map.elems $ Map.filter ((nm ==) . author . fst) ps)
    pure (Map.fromList aps)

{-# NOINLINE rawAuthorPackages #-}
rawAuthorPackages :: Cached (Map Name BSL.ByteString)
rawAuthorPackages = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached authorPackages

{-# NOINLINE packageVersions #-}
packageVersions :: Cached (Map (PackageName,Types.Version) (Package.Version Rendered))
packageVersions = unsafePerformIO $ 
  forkCache epoch $
    build <$!>
      loadVersions
  where
    build = Map.fromList . fmap process
      where
        process (pkgy,py) = (key,value) 
          where
            nm :: Package.Package -> PackageName
            nm = name

            ver :: Package.Version Txt -> Types.Version
            ver = version

            key = (nm pkgy,ver py)
            value = fmap parseMarkdown py

    loadVersions = catMaybes . concat . catMaybes <$!> do
      globs                 [i|static/packages/*/|] $ \p  ->
        withYamlFile        [i|#{p}package.yaml|]  $ \pkgy ->
          globs             [i|#{p}versions/*/|]   $ \d ->
            withYamlFile    [i|#{d}version.yaml|]  $ \vy ->
              pure (pkgy,vy)

{-# NOINLINE rawPackageVersions #-}
rawPackageVersions :: Cached (Map (PackageName,Types.Version) BSL.ByteString)
rawPackageVersions = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached packageVersions

{-# NOINLINE packageVersionsList #-}
packageVersionsList :: Cached (Map PackageName [Package.Version Rendered])
packageVersionsList = unsafePerformIO $
  forkCache epoch $
    fmap (fmap snd) . pushdown id <$!>
      cached packageVersions

{-# NOINLINE rawPackageVersionsList #-}
rawPackageVersionsList :: Cached (Map PackageName BSL.ByteString)
rawPackageVersionsList = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached packageVersionsList

{-# NOINLINE packagePosts #-}
packagePosts :: Cached (Map (PackageName,Slug) (Post Rendered,PostContent Rendered))
packagePosts = unsafePerformIO $ 
  forkCache epoch $
    build <$!>
      loadPosts
  where
    build = Map.fromList . fmap process
      where
        process (pkgy,psty,m) = (key,value) 
          where
            nm :: Package.Package -> PackageName
            nm = name

            slg :: Post Txt -> Slug
            slg = slug

            key = (nm pkgy,slg psty)
            value = (fmap parseMarkdown psty,m)

    loadPosts = catMaybes . concat . catMaybes <$!> do
      globs                 [i|static/packages/*/|] $ \p  ->
        withYamlFile        [i|#{p}package.yaml|]  $ \pkgy ->
          globs             [i|#{p}blog/*/|]       $ \d ->
            withYamlFile    [i|#{d}post.yaml|]     $ \psty -> do
              m <- markdown [i|#{d}post.md|]
              pure (pkgy,psty,m)

{-# NOINLINE rawPackagePosts #-}
rawPackagePosts :: Cached (Map (PackageName,Slug) BSL.ByteString)
rawPackagePosts = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached packagePosts

{-# NOINLINE packagePostsList #-}
packagePostsList :: Cached (Map PackageName [Post Rendered])
packagePostsList = unsafePerformIO $ 
  forkCache epoch $
    fmap (fmap (fst . snd)) . pushdown id <$!> 
      cached packagePosts

{-# NOINLINE rawPackagePostsList #-}
rawPackagePostsList :: Cached (Map PackageName BSL.ByteString)
rawPackagePostsList = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached packagePostsList

{-# NOINLINE packagePostsContent #-}
packagePostsContent :: Cached (Map (PackageName,Slug) (PostContent Rendered))
packagePostsContent = unsafePerformIO $
  forkCache epoch $
    fmap snd <$!>
      cached packagePosts

{-# NOINLINE rawPackagePostsContent #-}
rawPackagePostsContent :: Cached (Map (PackageName,Slug) BSL.ByteString)
rawPackagePostsContent = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached packagePostsContent

{-# NOINLINE packageTutorials #-}
packageTutorials :: Cached (Map (PackageName,Types.Version,Slug) (Tutorial Rendered,TutorialContent Rendered))
packageTutorials = unsafePerformIO $ 
  forkCache epoch $
    build <$!>
      loadPackageTutorials
  where
    build = Map.fromList . fmap process
      where
        process (py,vy,ty,t) = (key,value)
          where
            nm :: Package.Package -> PackageName
            nm = name

            slg :: Tutorial Txt -> Slug
            slg = slug

            ver :: Package.Version Txt -> Types.Version
            ver = version

            key = (nm py,ver vy,slg ty)
            value = (fmap parseMarkdown ty,t)

    loadPackageTutorials = catMaybes . concat . catMaybes . concat . catMaybes <$!> do
      globs                     [i|static/packages/*/|] $ \p  ->
        withYamlFile            [i|#{p}package.yaml|]  $ \py ->
          globs                 [i|#{p}versions/*/|]   $ \v  ->
            withYamlFile        [i|#{v}version.yaml|]  $ \vy ->
              globs             [i|#{v}tutorials/*/|]  $ \t  ->
                withYamlFile    [i|#{t}tutorial.yaml|] $ \ty -> do
                  m <- markdown [i|#{t}tutorial.md|]
                  pure (py,vy,ty,m)

{-# NOINLINE rawPackageTutorials #-}
rawPackageTutorials :: Cached (Map (PackageName,Types.Version,Slug) BSL.ByteString)
rawPackageTutorials = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached packageTutorials

{-# NOINLINE packageTutorialsList #-}
packageTutorialsList :: Cached (Map (PackageName,Types.Version) [Tutorial Rendered])
packageTutorialsList = unsafePerformIO $ 
  forkCache epoch $
    fmap (fmap (fst . snd)) . pushdown (\(a,b,c) -> ((a,b),c)) <$!> 
      cached packageTutorials

{-# NOINLINE rawPackageTutorialsList #-}
rawPackageTutorialsList :: Cached (Map (PackageName,Types.Version) BSL.ByteString)
rawPackageTutorialsList = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached packageTutorialsList

{-# NOINLINE packageTutorialsContent #-}
packageTutorialsContent :: Cached (Map (PackageName,Types.Version,Slug) (TutorialContent Rendered))
packageTutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap snd <$!>
      cached packageTutorials

{-# NOINLINE rawPackageTutorialsContent #-}
rawPackageTutorialsContent :: Cached (Map (PackageName,Types.Version,Slug) BSL.ByteString)
rawPackageTutorialsContent = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached packageTutorialsContent

-- implementation at bottom of file for parsing reasons
{-# NOINLINE modules #-}
modules :: Cached (Map (PackageName,Types.Version,ModuleName) (Module Rendered,ModuleContent Rendered))

{-# NOINLINE rawModules #-}
rawModules :: Cached (Map (PackageName,Types.Version,ModuleName) BSL.ByteString)
rawModules = unsafePerformIO $ 
  forkCache epoch $
    fmap (encodeBS . fst) <$!> 
      cached modules

{-# NOINLINE modulesList #-}
modulesList :: Cached (Map (PackageName,Types.Version) [Module Rendered])
modulesList = unsafePerformIO $ 
  forkCache epoch $
    fmap (fmap (fst . snd)) . pushdown (\(a,b,c) -> ((a,b),c)) <$!> 
      cached modules

{-# NOINLINE rawModulesList #-}
rawModulesList :: Cached (Map (PackageName,Types.Version) BSL.ByteString)
rawModulesList = unsafePerformIO $ 
  forkCache epoch $
    fmap encodeBS <$!> 
      cached modulesList

{-# NOINLINE rawModulesContent #-}
rawModulesContent :: Cached (Map (PackageName,Types.Version,ModuleName) BSL.ByteString)
rawModulesContent = unsafePerformIO $
  forkCache epoch $
    fmap (encodeBS . snd) <$!>
      cached modules

{-# NOINLINE modulesContentList #-}
modulesContentList :: Cached (Map (PackageName,Types.Version) [(Module Rendered,ModuleContent Rendered)])
modulesContentList = unsafePerformIO $
  forkCache epoch $
    fmap (fmap snd) . pushdown (\(a,b,c) -> ((a,b),c)) <$!>
      cached modules

{-# NOINLINE rawModulesContentList #-}
rawModulesContentList :: Cached (Map (PackageName,Types.Version) BSL.ByteString)
rawModulesContentList = unsafePerformIO $
  forkCache epoch $
    fmap encodeBS <$!>
      cached modulesContentList

-- Helpers

{-# INLINE (<$!>) #-}
infixl 4 <$!>
(<$!>) :: Functor f => (a -> b) -> f a -> f b
(<$!>) f fa = fmap g fa
  where g !a = let !b = f a in b

withYamlFile :: (FromJSON a) => FilePath -> (a -> IO b) -> IO (Maybe b)
withYamlFile fp f = do
  c <- BS.readFile fp
  BS.length c `seq`
    case Yaml.decodeEither' c of
      Left e ->
        putStrLn [i|Bad yaml file at #{fp} with error #{show e}|]
          *> hFlush stdout
          *> pure Nothing
      Right a ->
        Just <$!> f a

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
-- This appoach is unsafe: the results must be stable w.r.t. the original ordering
--
-- surprisingly good theoretical complexity characteristics:
--   Map.toAscList (O(n)) -> fmap _ (O(n)) -> Map.fromAscListWith (O(n)) 
-- The intermediate lists may all fuse away, too
{-# INLINE pushdown #-}
pushdown :: Ord l => (k -> (l,w)) -> Map k v -> Map l [(w,v)]
pushdown f = Map.fromAscListWith (flip (++)) . fmap go . Map.toAscList
  where go (k,v) = let (l,w) = f k in (l,[(w,v)])

{-# INLINE splits #-}
splits :: Ord l => (k -> v -> [(l,w)]) -> Map k v -> Map l [w]
splits f = Map.fromListWith (flip (++)) . List.concatMap go . Map.toAscList
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
            nm :: Package.Package -> PackageName
            nm = name

            mdl :: Module Txt -> ModuleName
            mdl = name

            ver :: Package.Version Txt -> Types.Version
            ver = version

            key   = (nm py,ver vy,mdl my)
            value = (fmap parseMarkdown my,md)

    loadModules = catMaybes . concat . catMaybes . concat . catMaybes <$!> do
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
        request compileAPI tryWS compile (Txt.dropWhile isSpace t,True) print
    example v = traverse_ example (getChildren v)

texts :: [View] -> Txt
texts = Txt.dropWhile isSpace . List.foldl' append ""
  where
    append t (TextView _ a) = t <> "\n" <> (Txt.dropWhileEnd isSpace a)
    append t _ = t
