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

import Pure.Data.JSON (encodeBS,FromJSON)
import Pure.Data.Render ()
import Pure.Data.View (View)
import Pure.Data.Txt as Txt
    ( dropWhile, dropWhileEnd, repack )
import Pure.Data.Txt.Interpolate ( i )
import Pure.TagSoup ( parseView )
import Pure.WebSocket as WS ( request, clientWS, WebSocket )

import Data.Char ( isSpace )
import qualified Data.ByteString as BS ( length, readFile )
import qualified Data.ByteString.Lazy as BSL ( ByteString )
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
import GHC.Stack (withFrozenCallStack, callStack, getCallStack, HasCallStack)

epoch :: Time
epoch = Seconds 10 0

-- Caches and their reverse-dependency trees.
--
-- authors
--   => rawAuthors
--   => authorsList
--      => rawAuthorsList
--      => authorPackages
--         => rawAuthorPackages
--   => authorsContent
--      => rawAuthorsContent
--
-- posts
--   => authorPosts
--      => rawAuthorsPosts
--
-- tutorials
--   => rawTutorials
--   => rawTutorialsList
--   => tutorialsContent
--      => rawTutorialsContent
--   => authorTutorials
--      => rawAuthorTutorials
--
-- pages
--   => rawPages
--   => pagesList
--      => rawPagesList
--   => pagesContent
--      => rawPagesContent
--
-- posts
--   => rawPosts
--   => postsList
--      => rawPostsList
--   => postsContent
--      => rawPostsContent
--
-- packages
--   => rawPackages
--   => rawPackageContents
--   => packagesList
--      => rawPackagesList
--   => authorPackages
--      => rawAuthorPackages
--
-- packageVersions
--   => rawPackageVersions
--   => packageVersionsList
--      => rawPackageVersionsList
--
-- packagePosts
--   => rawPackagePosts
--   => packagePostsList
--      => rawPackagePostsList
--   => packagePostsContent
--      => rawPackagePostsContent
--
-- packageTutorials
--   => rawPackageTutorials
--   => packageTutorialsList
--      => rawPackageTutorialsList
--   => packageTutorialsContent
--      => rawPackageTutorialsContent
--
-- modules
--   => rawModules
--   => modulesList
--      => rawModulesList
--   => rawModulesContent
--   => modulesContentList
--      => rawModulesContentList

{-# NOINLINE authors #-}
authors :: Map Name (Author Rendered,AuthorContent Rendered)
authors = unsafePerformIO (build <$> loadAuthors)
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
rawAuthors :: Map Name BSL.ByteString
rawAuthors = fmap (encodeBS . fst) Services.Caches.authors

{-# NOINLINE authorsList #-}
authorsList :: [Author Rendered]
authorsList = fmap fst (Map.elems Services.Caches.authors)

{-# NOINLINE rawAuthorsList #-}
rawAuthorsList :: BSL.ByteString
rawAuthorsList = encodeBS authorsList

{-# NOINLINE authorsContent #-}
authorsContent :: Map Name (AuthorContent Rendered)
authorsContent = fmap snd Services.Caches.authors

{-# NOINLINE rawAuthorsContent #-}
rawAuthorsContent :: Map Name BSL.ByteString
rawAuthorsContent = fmap encodeBS authorsContent

{-# NOINLINE authorPosts #-}
authorPosts :: Map Name [Post Rendered]
authorPosts = rebuild posts
  where
    rebuild :: Map Slug (Post Rendered,PostContent Rendered) -> Map Name [Post Rendered]
    rebuild = splits (\_ (pv,_) -> fmap (\a -> (a,pv)) (toList ((Blog.authors :: Post Rendered -> Authors) pv)))

{-# NOINLINE rawAuthorPosts #-}
rawAuthorPosts :: Map Name BSL.ByteString
rawAuthorPosts = fmap encodeBS authorPosts

{-# NOINLINE authorTutorials #-}
authorTutorials :: Map Name [Tutorial Rendered]
authorTutorials = rebuild tutorials
  where
    rebuild :: Map Slug (Tutorial Rendered,TutorialContent Rendered) -> Map Name [Tutorial Rendered]
    rebuild = splits (\_ (tv,_) -> fmap (\a -> (a,tv)) (toList ((Tutorial.authors :: Tutorial Rendered -> Authors) tv)))

{-# NOINLINE rawAuthorTutorials #-}
rawAuthorTutorials :: Map Name BSL.ByteString
rawAuthorTutorials = fmap encodeBS authorTutorials

{-# NOINLINE pages #-}
pages :: Map Slug (Page,PageContent Rendered)
pages = unsafePerformIO $ 
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

    loadPages = catMaybes <$> do
      globs             [i|static/pages/*/|] $ \d  ->
        withYamlFile    [i|#{d}/page.yaml|]  $ \py -> do
          m <- markdown [i|#{d}/page.md|]
          pure (py,m)

{-# NOINLINE rawPages #-}
rawPages :: Map Slug BSL.ByteString
rawPages = fmap (encodeBS . fst) pages

{-# NOINLINE pagesList #-}
pagesList :: [Page]
pagesList = fmap fst (Map.elems pages)

{-# NOINLINE rawPagesList #-}
rawPagesList :: BSL.ByteString
rawPagesList = encodeBS pagesList

{-# NOINLINE pagesContent #-}
pagesContent :: Map Slug (PageContent Rendered)
pagesContent = fmap snd pages

{-# NOINLINE rawPagesContent #-}
rawPagesContent :: Map Slug BSL.ByteString
rawPagesContent = fmap encodeBS pagesContent

{-# NOINLINE posts #-}
posts :: Map Slug (Post Rendered,PostContent Rendered)
posts = unsafePerformIO (build <$> loadPosts)
  where
    build = Map.fromList . fmap process
      where

        process (py,m) = (key,value)
          where 
            slg :: Post Txt -> Slug
            slg = slug
            
            key = slg py
            value = (fmap parseMarkdown py,m)


    loadPosts = catMaybes <$> do
      globs             [i|static/blog/*/|] $ \d  ->
        withYamlFile    [i|#{d}post.yaml|] $ \py -> do
          m <- markdown [i|#{d}post.md|]
          pure (py,m)

{-# NOINLINE rawPosts #-}
rawPosts :: Map Slug BSL.ByteString
rawPosts = fmap (encodeBS . fst) posts

{-# NOINLINE postsList #-}
postsList :: [Post Rendered]
postsList = fmap fst (Map.elems posts)

{-# NOINLINE rawPostsList #-}
rawPostsList :: BSL.ByteString
rawPostsList = encodeBS postsList 

{-# NOINLINE postsContent #-}
postsContent :: Map Slug (PostContent Rendered)
postsContent = fmap snd posts

{-# NOINLINE rawPostsContent #-}
rawPostsContent :: Map Slug BSL.ByteString
rawPostsContent = fmap encodeBS postsContent

{-# NOINLINE tutorials #-}
tutorials :: Map Slug (Tutorial Rendered,TutorialContent Rendered)
tutorials = unsafePerformIO (build <$> loadTutorials)
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
rawTutorials :: Map Slug BSL.ByteString
rawTutorials = fmap (encodeBS . fst) tutorials

{-# NOINLINE rawTutorialsList #-}
rawTutorialsList :: BSL.ByteString
rawTutorialsList = encodeBS . fmap fst $ Map.elems tutorials

{-# NOINLINE tutorialsContent #-}
tutorialsContent :: Map Slug (TutorialContent Rendered)
tutorialsContent = fmap snd tutorials

{-# NOINLINE rawTutorialsContent #-}
rawTutorialsContent :: Map Slug BSL.ByteString
rawTutorialsContent = fmap encodeBS tutorialsContent

{-# NOINLINE packages #-}
packages :: Map PackageName (Package,PackageContent Rendered)
packages = unsafePerformIO (build <$> loadPackages)
  where
    build = Map.fromList . fmap process
      where
        nm :: Package.Package -> PackageName
        nm = name

        process (py,pm) = (nm py,(py,pm))

    loadPackages = catMaybes <$> do
      globs          [i|static/packages/*/|] $ \p -> do
        withYamlFile [i|#{p}package.yaml|] $ \yml -> do
          md  <- markdown [i|#{p}package.md|] 
          pure (yml,md)

{-# NOINLINE rawPackages #-}
rawPackages :: Map PackageName BSL.ByteString
rawPackages = fmap (encodeBS . fst) Services.Caches.packages

{-# NOINLINE rawPackageContents #-}
rawPackageContents :: Map PackageName BSL.ByteString
rawPackageContents = fmap (encodeBS . snd) Services.Caches.packages

{-# NOINLINE packagesList #-}
packagesList :: [Package]
packagesList = fmap fst (Map.elems Services.Caches.packages)

{-# NOINLINE rawPackagesList #-}
rawPackagesList :: BSL.ByteString
rawPackagesList = encodeBS packagesList

{-# NOINLINE authorPackages #-}
authorPackages :: Map Name [Package]
authorPackages = Map.fromList $ flip fmap authorsList $ \a -> 
    let nm = Author.name a
    in (nm,fmap fst $ Map.elems $ Map.filter ((nm ==) . author . fst) Services.Caches.packages)

{-# NOINLINE rawAuthorPackages #-}
rawAuthorPackages :: Map Name BSL.ByteString
rawAuthorPackages = fmap encodeBS authorPackages

{-# NOINLINE packageVersions #-}
packageVersions :: Map (PackageName,Types.Version) (Package.Version Rendered)
packageVersions = unsafePerformIO (build <$> loadVersions)
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

    loadVersions = catMaybes . concat . catMaybes <$> do
      globs                 [i|static/packages/*/|] $ \p  ->
        withYamlFile        [i|#{p}package.yaml|]  $ \pkgy ->
          globs             [i|#{p}versions/*/|]   $ \d ->
            withYamlFile    [i|#{d}version.yaml|]  $ \vy ->
              pure (pkgy,vy)

{-# NOINLINE rawPackageVersions #-}
rawPackageVersions :: Map (PackageName,Types.Version) BSL.ByteString
rawPackageVersions = fmap encodeBS packageVersions

{-# NOINLINE packageVersionsList #-}
packageVersionsList :: Map PackageName [Package.Version Rendered]
packageVersionsList = fmap (fmap snd) (pushdown id packageVersions)

{-# NOINLINE rawPackageVersionsList #-}
rawPackageVersionsList :: Map PackageName BSL.ByteString
rawPackageVersionsList = fmap encodeBS packageVersionsList

{-# NOINLINE packagePosts #-}
packagePosts :: Map (PackageName,Slug) (Post Rendered,PostContent Rendered)
packagePosts = unsafePerformIO (build <$> loadPosts)
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

    loadPosts = catMaybes . concat . catMaybes <$> do
      globs                 [i|static/packages/*/|] $ \p  ->
        withYamlFile        [i|#{p}package.yaml|]  $ \pkgy ->
          globs             [i|#{p}blog/*/|]       $ \d ->
            withYamlFile    [i|#{d}post.yaml|]     $ \psty -> do
              m <- markdown [i|#{d}post.md|]
              pure (pkgy,psty,m)

{-# NOINLINE rawPackagePosts #-}
rawPackagePosts :: Map (PackageName,Slug) BSL.ByteString
rawPackagePosts = fmap (encodeBS . fst) packagePosts

{-# NOINLINE packagePostsList #-}
packagePostsList :: Map PackageName [Post Rendered]
packagePostsList = fmap (fmap (fst . snd)) (pushdown id packagePosts)

{-# NOINLINE rawPackagePostsList #-}
rawPackagePostsList :: Map PackageName BSL.ByteString
rawPackagePostsList = fmap encodeBS packagePostsList

{-# NOINLINE packagePostsContent #-}
packagePostsContent :: Map (PackageName,Slug) (PostContent Rendered)
packagePostsContent = fmap snd packagePosts

{-# NOINLINE rawPackagePostsContent #-}
rawPackagePostsContent :: Map (PackageName,Slug) BSL.ByteString
rawPackagePostsContent = fmap encodeBS packagePostsContent

{-# NOINLINE packageTutorials #-}
packageTutorials :: Map (PackageName,Types.Version,Slug) (Tutorial Rendered,TutorialContent Rendered)
packageTutorials = unsafePerformIO (build <$> loadPackageTutorials)
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
rawPackageTutorials :: Map (PackageName,Types.Version,Slug) BSL.ByteString
rawPackageTutorials = fmap (encodeBS . fst) packageTutorials

{-# NOINLINE packageTutorialsList #-}
packageTutorialsList :: Map (PackageName,Types.Version) [Tutorial Rendered]
packageTutorialsList = fmap (fmap (fst . snd)) (pushdown (\(a,b,c) -> ((a,b),c)) packageTutorials)

{-# NOINLINE rawPackageTutorialsList #-}
rawPackageTutorialsList :: Map (PackageName,Types.Version) BSL.ByteString
rawPackageTutorialsList = fmap encodeBS packageTutorialsList

{-# NOINLINE packageTutorialsContent #-}
packageTutorialsContent :: Map (PackageName,Types.Version,Slug) (TutorialContent Rendered)
packageTutorialsContent = fmap snd packageTutorials

{-# NOINLINE rawPackageTutorialsContent #-}
rawPackageTutorialsContent :: Map (PackageName,Types.Version,Slug) BSL.ByteString
rawPackageTutorialsContent = fmap encodeBS packageTutorialsContent

-- implementation at bottom of file for parsing reasons
{-# NOINLINE modules #-}
modules :: Map (PackageName,Types.Version,ModuleName) (Module Rendered,ModuleContent Rendered)
modules = unsafePerformIO (build <$> loadModules)
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

    loadModules = catMaybes . concat . catMaybes . concat . catMaybes <$> do
      globs                     [i|static/packages/*/|] $ \p  ->
        withYamlFile            [i|#{p}package.yaml|]  $ \py ->
          globs                 [i|#{p}versions/*/|]   $ \v  ->
            withYamlFile        [i|#{v}version.yaml|]  $ \vy ->
              globs             [i|#{v}modules/*/|]    $ \m  ->
                withYamlFile    [i|#{m}module.yaml|]  $ \my -> do
                  md <- markdown [i|#{m}module.md|]
                  pure (py,vy,my,md)

{-# NOINLINE rawModules #-}
rawModules :: Map (PackageName,Types.Version,ModuleName) BSL.ByteString
rawModules = fmap (encodeBS . fst) modules

{-# NOINLINE modulesList #-}
modulesList :: Map (PackageName,Types.Version) [Module Rendered]
modulesList = fmap (fmap (fst . snd)) (pushdown (\(a,b,c) -> ((a,b),c)) modules)

{-# NOINLINE rawModulesList #-}
rawModulesList :: Map (PackageName,Types.Version) BSL.ByteString
rawModulesList = fmap encodeBS modulesList

{-# NOINLINE rawModulesContent #-}
rawModulesContent :: Map (PackageName,Types.Version,ModuleName) BSL.ByteString
rawModulesContent = fmap (encodeBS . snd) modules

{-# NOINLINE modulesContentList #-}
modulesContentList :: Map (PackageName,Types.Version) [(Module Rendered,ModuleContent Rendered)]
modulesContentList = fmap (fmap snd) (pushdown (\(a,b,c) -> ((a,b),c)) modules)

{-# NOINLINE rawModulesContentList #-}
rawModulesContentList :: Map (PackageName,Types.Version) BSL.ByteString
rawModulesContentList = fmap encodeBS modulesContentList

-- Helpers

withYamlFile :: (FromJSON a) => FilePath -> (a -> IO b) -> IO (Maybe b)
withYamlFile fp f = do
  c <- BS.readFile fp
  case Yaml.decodeEither' c of
    Left e -> pure Nothing
      -- putStrLn [i|Bad yaml file at #{fp} with error #{show e}|]
      --  *> hFlush stdout
      --  *> pure Nothing
    Right a ->
      Just <$> f a

parseMarkdown :: Txt -> Rendered
parseMarkdown cnt =
    let Right result = Text.Pandoc.Class.runPure $ do
          md <- readMarkdown
                   Pandoc.def
                     { Pandoc.readerExtensions = Pandoc.pandocExtensions }
                   (Txt.repack cnt)
          str <- writeHtml5String Pandoc.def md
          let view = parseView str
          pure view
    in Rendered result

markdown :: (FromTxt (f Txt), Functor f, Foldable f) => FilePath -> IO (f Rendered)
markdown fp = do
  f <- BS.readFile fp
  let md = fmap parseMarkdown (fromTxt (toTxt f))
  traverse_ processExamples md
  pure md 

globs :: FilePath -> (FilePath -> IO a) -> IO [a]
globs p f = Glob.glob p >>= traverse f

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

{-# NOINLINE tryWS #-}
tryWS :: WebSocket
tryWS = unsafePerformIO $ clientWS "204.48.20.19" 8080

processExamples :: Rendered -> IO ()
processExamples (Rendered md) = traverse_ example md
  where
    example :: View -> IO ()
    example (Children (texts -> t) (Attributes as Pre)) 
      | Just _ <- List.lookup "data-try" as = void $
        request compileAPI tryWS compile (Txt.dropWhile isSpace t,True) (const (return ())) -- print
    example v = traverse_ example (getChildren v)

texts :: [View] -> Txt
texts = Txt.dropWhile isSpace . List.foldl' append ""
  where
    append t (TextView _ a) = t <> "\n" <> (Txt.dropWhileEnd isSpace a)
    append t _ = t
