module Main where

import Pure hiding (modify,get,none,Transform)
import Pure.Data.Styles
import Pure.Data.CSS
import Pure.Theme

import Pure.Data.SVG
import Pure.Data.SVG.Properties

import Shared.Colors
import Shared.Styles
import Shared.Components.GitHubLogo
import Shared.Components.Logo

import Router
import Scope hiding (modify,get,contains,(#),transform,none,has)
import qualified Scope

import Pages.Examples
import Pages.Home

import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Control.Monad.Trans.State as St
import Data.Foldable

import Pure.Cache

setup :: AppScope => IO ()
setup = return ()

pages :: PageScope => Route -> View
pages pg =
  case pg of

    NoR               -> Null

    HomeR             -> homePage

    BlogR             -> blogPage
    PostR y m d s     -> postPage y m d s

    DocsR             -> docsPage
    DocR n g nm       -> docPage n g nm

    TutsR             -> tutorialsPage
    TutorialR g c n t -> tutorialPage g c n t

    ExamplesR         -> examplesPage
    ExampleR nm       -> Null


-- MAIN
main :: IO ()
main = do
  now <- time
  Scope.run (State now) NoR Router.router setup id pages

blogPage = Null
postPage _ _ _ _ = Null

docsPage = Null
docPage _ _ _ = Null

tutorialsPage = Null
tutorialPage _ _ _ _ = Null

--     has ".header" .> do
--       backgroundColor =: initial

-- 
-- ---- BLOG
-- data BlogState ms = BlogState
--   { bsPost :: Maybe (Txt,Txt,Txt,Txt)
--   , bsPostsToggled :: Bool
--   } deriving Typeable

-- _Blog = Controller {..}
--   where
--     key = "Blog"
--     build = return
--     prime = void $
--       remote getFeaturedPost () $ traverse_ $ \mp -> do
--         for mp $ \p -> liftIO $ do
--           with (_Post (pmPath $ pMeta p) mp) (return ())
--           with _PostMetaList (setSelectedPost $ pmPath $ pMeta p)
--           putLocalItemIfNotExists (buildPostPath $ pmPath (pMeta p)) p
--         modifyModel $ \BlogState {..} -> BlogState { bsPost = fmap (pmPath . pMeta) mp, .. }
--     model = BlogState Nothing False
--     view BlogState {..} =
--       Div
--         [ ClassList [ "Blog" ] ]
--         [ Hdr (Just BlogActive)
--         , may (mvc Article [ ClassList [ "main" ] ] . flip _Post Nothing) bsPost
--         , mvc Div [] _PostMetaList
--         ]

-- loadPost y m d s = do
--   mres <- demandMaybe =<< getLocalItem (buildPostPath (y,m,d,s))
--   with (_Post (y,m,d,s) (snd <$> join mres)) (return ())
--   modifyModel $ \BlogState {..} -> BlogState { bsPost = Just (y,m,d,s), .. }

-- blogStyles = do
--   is ".Blog" $ do
--     apply $
--       backgroundColor =: baseWhite

--     has ".header" .> do
--       position  =: fixed

--     has ".main" $ do
--       apply $ do
--         minHeight   =: calc(vhs 100 <<->> pxs 60)
--         maxWidth    =: per 100
--         padding     =: ems 1

--       atMedia "(max-width: 48em)" .> do
--         height      =: calc(vhs 100 <<->> pxs 40)

--       atMedia "(min-width: 48em)" .> do
--         marginLeft  =: ems 1
--         marginRight =: calc(pxs 250)

--   blogFeaturesStyles

-- 
-- ---- POST
-- data PostState ms = PostState
--   { psPost :: Maybe Post
--   } deriving Typeable

-- _Post (y,m,d,t) mp = Controller {..}
--   where
--     key = "Post_" <> fromTxt y <> fromTxt m <> fromTxt d <> fromTxt t
--     build = return
--     prime = do
--       PostState {..} <- getModel
--       unless (isJust psPost) $
--         void $ remote getPost (y,m,d,t) (traverse_ setPost)
--     model = PostState mp
--     view PostState {..} =
--       flip may psPost $ \Post {..} ->
--         Div [ ClassList [ "Post" ] ]
--           [ css (for_ (pmHighlights pMeta) (uncurry highlight))
--           , H1 [ ClassList [ "title" ] ] [ fromTxt (pmTitle pMeta) ]
--           , Span [ ClassList [ "byline" ] ]
--               [ View $ Render $ pmPathToDate $ pmPath pMeta
--               , " by "
--               , Span [ ClassList [ "author" ] ] [ fromTxt (pmAuthor pMeta) ]
--               ]
--           , Div [ ClassList [ "md" ] ]
--               (witnesses pContent)
--           ]
--       where
--         pmPathToDate (y,m,d,_) = Date (y,m,d)

--         setPost mp = modifyModel $ \PostState {..} -> PostState { psPost = mp, .. }

-- postStyles = do

--   is ".Post" $ do

--     apply $ do
--       marginTop   =: pxs 45
--       maxWidth    =: pxs 1260
--       marginLeft  =: auto
--       marginRight =: auto

--     atMedia "(min-width: 48em)" .> do
--       marginTop =: pxs 60

--     has "h1" .> do
--       marginBottom =: zero
--       fontSize     =: pxs 40
--       fontWeight   =: int 700

--     has ".byline" .> do
--       return ()

-- 
-- ---- POSTMETALIST
-- data PostMetaListState ms = PostMetaListState
--   { pmlsMetas :: [PostMeta]
--   , pmlsSelected :: (Txt,Txt,Txt,Txt)
--   , pmlsToggled :: Bool
--   , pmlsCurrent :: Maybe Txt
--   } deriving Typeable

-- _PostMetaList = Controller {..}
--   where
--     key = "PostMetaList"
--     build = return
--     prime = void $ do
--       remote getPostMetas () $ \pms ->
--         modifyModel $ \PostMetaListState {..} ->
--           PostMetaListState { pmlsMetas = fromMaybe [] pms, .. }
--     model = PostMetaListState def def def (Just "Recent Posts")
--     view PostMetaListState {..} =
--       Div
--         [ ClassList [ "PostMetaList" ] ]
--         [ View $ Index pmlsToggled pmlsCurrent onSel
--             [("Recent Posts",flip map pmlsMetas $ \pm ->
--                View $ PostMetaListItem pm (pmlsSelected == pmPath pm) $ do
--                  togglePostMetaList (const False)
--                  setSelectedPost (pmPath pm)
--              )]
--         , View (Toggle pmlsToggled (togglePostMetaList not))
--         ]

--       where
--         onSel txt = modifyModel $ \PostMetaListState {..} ->
--           let pmlsCurrent' = if pmlsCurrent == Just txt then Nothing else Just txt
--           in PostMetaListState { pmlsCurrent = pmlsCurrent', .. }

-- togglePostMetaList f =
--   modifyModel $ \PostMetaListState {..} ->
--     PostMetaListState { pmlsToggled = f pmlsToggled, .. }

-- setSelectedPost pm =
--   modifyModel $ \PostMetaListState {..} ->
--     PostMetaListState { pmlsSelected = pm, .. }

-- postMetaListStyles = do

--   is ".PostMetaList" .>
--     return ()

-- 
-- ------ POSTMETALISTITEM
-- data PostMetaListItem ms = PostMetaListItem
--   { pmliPostMeta :: PostMeta
--   , pmliSelected :: Bool
--   , pmliOnToggle :: Ef ms IO ()
--   }

-- instance Typeable ms => Pure PostMetaListItem ms where
--   render PostMetaListItem {..} = let PostMeta {..} = pmliPostMeta in
--     Div
--       [ ClassList
--           [ "post-meta-list-item-meta"
--           , pmliSelected # "post-meta-list-item-selected"
--           ]
--       ]
--       [ A
--           [ ClassList [ "post-meta-list-item-link" ]
--           , Lref (buildPostPath pmPath)
--           , onClick pmliOnToggle
--           ]
--           [ Div
--               [ ClassList [ "post-meta-list-item-title" ] ]
--               [ fromTxt pmTitle ]
--           ]
--       ]

-- buildPostPath (y,m,d,t) = "/blog/" <> y <> "/" <> m <> "/" <> d <> "/" <> t

-- postMetaListItemStyles = do
--   is ".post-meta-list-item-selected" $ do
--     apply $ do
--       borderLeft =: pxs 3 <<>> solid <<>> baseGreen

--     has ".post-meta-list-item-title" .> do
--       color =: darkGray
--       fontWeight =: bold

--   is ".post-meta-list-item-meta" .> do
--     marginLeft =: zero
--     marginTop =: zero
--     paddingLeft =: calc(ems 0.5 <<->> pxs 3)

--   is ".post-meta-list-item-link" .> do
--     textDecoration =: none
--     width =: pxs 250
--     whiteSpace =: nowrap
--     overflow =: hidden
--     textOverflow =: ellipsis
--     textDecoration =: none

--   is ".post-meta-list-item-title" $ do
--     apply $ do
--       color =: baseGray
--       margin =: zero
--       marginLeft =: ems 0.5
--       fontSize =: pxs 14

--     is hovered .>
--       color =: darkGray

-- 
-- ---- DOCS
-- data DocsState ms = DocsState
--   { dsDoc :: Maybe (Txt,Txt,Txt)
--   , dsHome :: Markdown
--   } deriving Typeable

-- _Docs = Controller {..}
--   where
--     key = "Docs"
--     build = return
--     prime = do
--       void $ remote getFeaturedDocumentation () $ traverse_ $ \md -> do
--         for md $ \d -> liftIO $ do
--           unsafePreinit (_Doc (dmPath $ dMeta d) md)
--           asDocList <- usingController _DocMetaList Preinit
--           asDocList (setSelectedDoc $ dmPath $ dMeta d)
--           asDocList (setCurrentDocGroup $ documentGroup $ dMeta d)
--           putLocalItemIfNotExists (buildDocPath $ dmPath (dMeta d)) d
--         DocsState {..} <- getModel
--         putModel DocsState { dsDoc = fmap (dmPath . dMeta) md, .. }
--     model = DocsState Nothing (Markdown [])
--     view DocsState {..} =
--       Div
--         [ ClassList [ "Docs" ] ]
--         [ Hdr (Just DocsActive)
--         , maybe
--             (Article [ ClassList [ "main", "home" ] ] [ View $ Render dsHome ])
--             (mvc Article [ ClassList [ "main" ] ] . flip _Doc Nothing)
--             dsDoc
--         , mvc Div [] _DocMetaList
--         ]

-- loadDocumentation n g nm = do
--   mres <- demandMaybe =<< getLocalItem (buildDocPath (n,g,nm))
--   liftIO $ print $ isJust $ join mres
--   with (_Doc (n,g,nm) (fmap snd $ join mres)) (return ())
--   modifyModel $ \DocsState {..} -> DocsState { dsDoc = Just (n,g,nm), .. }

-- docsStyles = do

--   is ".Docs" $ do
--     apply $ do
--       backgroundColor =: baseWhite

--     has ".main" $ do
--       apply $ do
--         marginTop   =: pxs 45
--         maxWidth    =: pxs 1260
--         marginLeft  =: auto
--         marginRight =: auto

--       atMedia "(min-width: 48em)" .> do
--         marginTop =: pxs 60

--       apply $ do
--         minHeight   =: calc(vhs 100 <<->> pxs 60)
--         maxWidth    =: per 100
--         padding     =: ems 1

--       atMedia "(max-width: 48em)" .> do
--         height      =: calc(vhs 100 <<->> pxs 40)

--       atMedia "(min-width: 48em)" .> do
--         marginLeft  =: ems 1
--         marginRight =: calc(pxs 250)

--       has ".home" $ do
--         apply $ do
--           marginTop   =: pxs 45
--           maxWidth    =: pxs 1260
--           marginLeft  =: auto
--           marginRight =: auto

--         atMedia "(min-width: 48em)" .> do
--           marginTop =: pxs 60

--         has "h1" .> do
--           marginBottom =: zero
--           fontSize     =: pxs 40
--           fontWeight   =: int 700

--         has ".byline" .> do
--           return ()

-- 
-- ---- DOC
-- data DocState ms = DocState
--   { dsMayDoc :: Maybe Doc
--   } deriving Typeable

-- _Doc (n,g,nm) md = Controller {..}
--   where
--     key = "Doc_" <> fromTxt n <> "-" <> fromTxt g <> "-" <> fromTxt nm
--     build = return
--     prime = loadDoc (n,g,nm)
--     model = DocState md
--     view DocState {..} =
--       flip may dsMayDoc $ \Doc {..} ->
--         Div [ ClassList [ "Doc" ] ]
--           [ css (for_ (dmHighlights dMeta) (uncurry highlight))
--           , H1 [ ClassList [ "title" ] ] [ fromTxt (dmTitle dMeta) ]
--           , Div [ ClassList [ "md" ] ]
--               (witnesses dContent)
--           ]

-- activateDoc md = do
--   for md $ \d ->
--     with _DocMetaList (setCurrentDocGroup (documentGroup (dMeta d)))
--   modifyModel $ \DocState {..} -> DocState { dsMayDoc = md, .. }

-- loadDoc (n,g,nm) = do
--   DocState {..} <- getModel
--   unless (isJust dsMayDoc) $
--     void $ remote getDoc (n,g,nm) (traverse_ activateDoc)

-- docStyles = do

--   is ".Doc" $ do

--     apply $ do
--       marginTop   =: pxs 45
--       maxWidth    =: pxs 1260
--       marginLeft  =: auto
--       marginRight =: auto

--     atMedia "(min-width: 48em)" .> do
--       marginTop =: pxs 60

--     has "h1" .> do
--       marginBottom =: zero
--       fontSize     =: pxs 40
--       fontWeight   =: int 700

-- 
-- ------ DOCMETALIST
-- data DocMetaListState ms = DocMetaListState
--   { dmlsMetas :: [DocMeta]
--   , dmlsSelected :: (Txt,Txt,Txt)
--   , dmlsToggled :: Bool
--   , dmlsCurrent :: Maybe Txt
--   } deriving Typeable

-- _DocMetaList = Controller {..}
--   where
--     key = "DocMetaList"
--     build = return
--     prime = void loadDocMetas
--     model = DocMetaListState def def def def
--     view DocMetaListState {..} =
--       Div
--         [ ClassList [ "DocsList" ] ]
--         [ View $ Index dmlsToggled dmlsCurrent onSel $ (fmap . fmap) (fmap renderItem) (groupAndSortDocs dmlsMetas)
--         , View (Toggle dmlsToggled (toggleDocMetaList not))
--         ]
--       where
--         onSel txt = modifyModel $ \DocMetaListState {..} ->
--           let dmlsCurrent' = if dmlsCurrent == Just txt then Nothing else Just txt
--           in DocMetaListState { dmlsCurrent = dmlsCurrent', .. }

--         renderItem dm =
--           View $ DocMetaListItem dm (dmlsSelected == dmPath dm) $ do
--             toggleDocMetaList (const False)
--             setCurrentDocGroup (documentGroup dm)
--             setSelectedDoc (dmPath dm)

--         groupAndSortDocs =
--           map (documentGroup . head &&& id) .
--           map (sortOn documentName) .
--           groupBy ((==) `on` documentGroup) .
--           sortOn documentNum

-- toggleDocMetaList f =
--   modifyModel $ \DocMetaListState {..} ->
--     DocMetaListState { dmlsToggled = f dmlsToggled, .. }

-- setSelectedDoc dm =
--   modifyModel $ \DocMetaListState {..} ->
--     DocMetaListState { dmlsSelected = dm, .. }

-- setCurrentDocGroup dg =
--   modifyModel $ \DocMetaListState {..} ->
--     DocMetaListState { dmlsCurrent = Just dg, .. }

-- loadDocMetas =
--   remote getDocMetas () $ \dms ->
--     modifyModel $ \DocMetaListState {..} ->
--       DocMetaListState { dmlsMetas = fromMaybe [] dms, .. }

-- docMetaListStyles = do

--   is ".DocMetaList" .> do
--     return ()

-- 
-- -------- DOCMETALISTITEM
-- data DocMetaListItem ms = DocMetaListItem
--   { dmliDocMeta  :: DocMeta
--   , dmliSelected :: Bool
--   , dmliOnToggle :: Ef ms IO ()
--   }

-- instance Typeable ms => Pure DocMetaListItem ms where
--   render DocMetaListItem {..} = let DocMeta {..} = dmliDocMeta in
--     Div
--     [ ClassList
--         [ "doc-meta-list-item-meta"
--         , dmliSelected # "doc-meta-list-item-selected"
--         ]
--     ]
--     [ A
--       [ ClassList [ "doc-meta-list-item-link" ]
--       , Lref (buildDocPath dmPath)
--       , onClick dmliOnToggle
--       ]
--       [ Div
--         [ ClassList [ "doc-meta-list-item-title" ] ]
--         [ fromTxt dmTitle ]
--       ]
--     ]

-- buildDocPath (n,g,nm) = "/doc/" <> n <> "/" <> g <> "/" <> nm

-- docMetaListItemStyles = do
--   is ".doc-meta-list-item-selected" $ do
--     apply $ do
--       borderLeft =: pxs 3 <<>> solid <<>> baseGreen

--     has ".doc-meta-list-item-title" .> do
--       color =: darkGray
--       fontWeight =: bold

--   is ".doc-meta-list-item-meta" .> do
--     marginLeft  =: zero
--     marginTop   =: zero
--     paddingLeft =: calc(ems 0.5 <<->> pxs 3)

--   is ".doc-meta-list-item-link" .> do
--     textDecoration =: none
--     width          =: pxs 250
--     whiteSpace     =: nowrap
--     overflow       =: hidden
--     textOverflow   =: ellipsis
--     textDecoration =: none

--   is ".doc-meta-list-item-title" $ do
--     apply $ do
--       color      =: baseGray
--       margin     =: zero
--       marginLeft =: ems 0.5
--       fontSize   =: pxs 14

--     is hovered .>
--       color =: darkGray

-- 
-- ---- TUTORIAL
-- data TutorialState ms = TutorialState
--   { tsTutorial :: Maybe (Txt,Txt,Txt,Txt)
--   } deriving Typeable

-- _Tutorial mtut = Controller {..}
--   where
--     key = "Tutorial"
--     build base = return (state (Map.empty :: HashMap Txt Tutorial) *:* base)
--     prime =
--       case mtut of
--         Just gcnt -> void $ sync $ loadTutorial gcnt
--         Nothing -> do
--           void $ remote getFeaturedTutorial () $ traverse_ $ \mt -> do
--             for mt $ \t -> liftIO $ do
--               unsafePreinit (_Tut (tmPath $ tMeta t))
--               asTutList <- usingController _TutMetaList Preinit
--               asTutList (setSelectedTut $ tmPath $ tMeta t)
--               asTutList (setCurrentTutGroup $ tutorialGroup $ tMeta t)
--               putLocalItemIfNotExists (buildTutPath (tmPath $ tMeta t)) t
--             modifyModel $ \TutorialState {..} ->
--               TutorialState { tsTutorial = fmap (tmPath . tMeta) mt, .. }
--     model = TutorialState mtut
--     view TutorialState {..} =
--       Div
--         [ ClassList [ "Tutorial" ] ]
--         [ Hdr (Just TutorialActive)
--         , may (mvc Article [ ClassList [ "main" ] ] . _Tut) tsTutorial
--         , mvc Div [] _TutMetaList
--         ]

-- loadTut gcnt@(buildTutPath -> tutPath) = do
--   tuts <- liftAsync get
--   case Map.lookup tutPath tuts of
--     Just t -> return $ Just t
--     Nothing -> do
--       mres <- Async $ getLocalItem tutPath
--       case mres of
--         Just (_,t) -> do
--           unsafePreinit (_Tut gcnt)
--           with (_Tut gcnt) (setTutorial t)
--           liftAsync $ put $ Map.insert tutPath t tuts
--           return (Just t)
--         Nothing -> do
--           mt <- Async $ withPromise $ \pr -> void $ remote getTutorial gcnt (void . fulfill pr)
--           case join mt of
--             Nothing -> return Nothing
--             Just t  -> do
--               putLocalItemIfNotExists (buildTutPath (tmPath $ tMeta t)) t
--               unsafePreinit (_Tut gcnt)
--               with (_Tut gcnt) (setTutorial t)
--               liftAsync $ put $ Map.insert tutPath t tuts
--               return (Just t)

-- loadTutorial gcnt = do
--   mtut <- loadTut gcnt
--   case mtut of
--     Nothing -> return ()
--     Just (tMeta -> tm) -> do
--       void $ with _TutMetaList $ do
--         setSelectedTut (tmPath tm)
--         setCurrentTutGroup (tutorialGroup tm)


-- tutorialStyles = do

--   is ".Tutorial" $ do
--     apply $ do
--       backgroundColor =: baseWhite

--     has ".main" $ do
--       apply $ do
--         minHeight   =: calc(vhs 100 <<->> pxs 60)
--         maxWidth    =: per 100
--         padding     =: ems 1

--       atMedia "(max-width: 48em)" .> do
--         height      =: calc(vhs 100 <<->> pxs 40)

--       atMedia "(min-width: 48em)" .> do
--         marginLeft  =: ems 1
--         marginRight =: calc(pxs 250)

-- 
-- ------ TUT
-- data TutState ms = TutState
--   { tsMayTut :: Maybe Tutorial
--   } deriving Typeable

-- _Tut (g,c,n,t) = Controller {..}
--   where
--     key = "Tut_" <> fromTxt g <> "-" <> fromTxt n <> "-" <> fromTxt c <> "-" <> fromTxt t
--     build = return
--     prime = return ()
--     model = TutState Nothing
--     view TutState {..} =
--       flip may tsMayTut $ \Tutorial {..} ->
--         Div [ ClassList [ "Tut" ] ]
--           [ css (for_ (tmHighlights tMeta) (uncurry highlight))
--           , H1 [ ClassList [ "title" ] ] [ fromTxt (tmTitle tMeta) ]
--           , Span [ ClassList [ "byline" ] ]
--               [ "By "
--               , Span [ ClassList [ "author" ] ] [ fromTxt (tmAuthor tMeta) ]
--               ]
--           , Div [ ClassList [ "md" ] ]
--               (witnesses tContent)
--           ]

-- setTutorial tut = modifyModel $ \TutState {..} -> TutState { tsMayTut = Just tut, .. }

-- -- activateTut mt = do
-- --   for mt $ \t ->
-- --     with _TutMetaList (setCurrentTutGroup (tutorialGroup (tMeta t)))
-- --   modifyModel $ \TutState {..} -> TutState { tsMayTut = mt, .. }

-- -- loadTut gcnt = do
-- --   liftIO $ print $ "activating: " ++ show gcnt
-- --   TutState {..} <- getModel
-- --   unless (isJust tsMayTut) $ do
-- --     mres <- demandMaybe =<< getLocalItem (buildTutPath gcnt)
-- --     case join mres of
-- --       Just (_,t) -> activateTut (Just t)
-- --       Nothing -> void $ remote getTutorial gcnt (traverse_ activateTut)

-- tutStyles = do

--   is ".Tut" $ do
--     apply $ do
--       marginTop   =: pxs 45
--       maxWidth    =: pxs 1260
--       marginLeft  =: auto
--       marginRight =: auto

--     atMedia "(min-width: 48em)" .> do
--       marginTop =: pxs 60

--     has "h1" .> do
--       marginBottom =: zero
--       fontSize     =: pxs 40
--       fontWeight   =: int 700

--     has ".byline" .> do
--       return ()

-- 
-- -------- TUTMETALIST
-- data TutMetaListState ms = TutMetaListState
--   { tmlsMetas :: [TutorialMeta]
--   , tmlsSelected :: (Txt,Txt,Txt,Txt)
--   , tmlsToggled :: Bool
--   , tmlsCurrent :: Maybe Txt
--   } deriving Typeable

-- _TutMetaList = Controller {..}
--   where
--     key = "TutMetaList"
--     build = return
--     prime = void loadTutMetas
--     model = TutMetaListState def def def def
--     view TutMetaListState {..} =
--         Div
--           [ ClassList [ "TutMetaList" ] ]
--           [ View $ Index tmlsToggled tmlsCurrent onSel $ (fmap . fmap) (fmap renderItem) (groupAndSortTuts tmlsMetas)
--           , View (Toggle tmlsToggled (toggleTutMetaList not))
--           ]
--       where
--         onSel txt = modifyModel $ \TutMetaListState {..} ->
--           let tmlsCurrent' = if tmlsCurrent == Just txt then Nothing else Just txt
--           in TutMetaListState { tmlsCurrent = tmlsCurrent', .. }

--         renderItem tm =
--           View $ TutMetaListItem tm (tmlsSelected == tmPath tm) $ do
--             toggleTutMetaList (const False)
--             setCurrentTutGroup (tutorialGroup tm)
--             setSelectedTut (tmPath tm)

--         groupAndSortTuts =
--           map (tutorialGroup . head &&& id) .
--           map (sortOn tutorialCh) .
--           groupBy ((==) `on` tutorialGroup) .
--           sortOn tutorialNum

-- toggleTutMetaList f =
--   modifyModel $ \TutMetaListState {..} ->
--     TutMetaListState { tmlsToggled = f tmlsToggled, .. }

-- setSelectedTut tm =
--   modifyModel $ \TutMetaListState {..} ->
--     TutMetaListState { tmlsSelected = tm, .. }

-- setCurrentTutGroup tg =
--   modifyModel $ \TutMetaListState {..} ->
--     TutMetaListState { tmlsCurrent = Just tg, .. }

-- loadTutMetas =
--   remote getTutorialMetas () $ \tms -> do
--     modifyModel $ \TutMetaListState {..} ->
--       TutMetaListState { tmlsMetas = fromMaybe [] tms, .. }

-- tutMetaListStyles = do

--   is ".TutMetaList" .> do
--     return ()

-- 
-- ---------- TUTMETALISTITEM
-- data TutMetaListItem ms = TutMetaListItem
--   { tmliTutorialMeta :: TutorialMeta
--   , tmliSelected     :: Bool
--   , tmliOnToggle     :: Ef ms IO ()
--   }

-- instance Typeable ms => Pure TutMetaListItem ms where
--   render TutMetaListItem {..} = let TutorialMeta {..} = tmliTutorialMeta in
--     Div
--       [ ClassList
--           [ "tut-meta-list-item-meta"
--           , tmliSelected # "tut-meta-list-item-selected"
--           ]
--       ]
--       [ A
--           [ ClassList [ "tut-meta-list-item-link" ]
--           , Lref (buildTutPath tmPath)
--           , onClick tmliOnToggle
--           ]
--           [ Div
--               [ ClassList [ "tut-meta-list-item-title" ] ]
--               [ fromTxt tmTitle ]
--           ]
--       ]

-- buildTutPath (group,chapter,name,title) = "/tutorial/" <> group <> "/" <> chapter <> "/" <> name <> "/" <> title

-- tutMetaListItemStyles = do

--   is ".tut-meta-list-item-selected" $ do
--     apply $ do
--       borderLeft =: pxs 3 <<>> solid <<>> baseGreen
--       paddingLeft =: calc(ems 0.5 <<->> pxs 3)

--     has ".tut-meta-list-item-title" .> do
--       color =: darkGray
--       fontWeight =: bold

--   is ".tut-meta-list-item-meta" .> do
--     marginLeft =: zero
--     marginTop =: zero
--     paddingLeft =: ems 0.5

--   is ".tut-meta-list-item-link" .> do
--     textDecoration =: none
--     width =: pxs 250
--     whiteSpace =: nowrap
--     overflow =: hidden
--     textOverflow =: ellipsis
--     textDecoration =: none

--   is ".tut-meta-list-item-title" $ do
--     apply $ do
--       color =: baseGray
--       margin =: zero
--       marginLeft =: ems 0.5
--       fontSize =: pxs 14

--     is hovered .>
--       color =: darkGray

-- 
-- -- FEATURES

-- 
-- ---- DATE
-- newtype Date = Date (Txt,Txt,Txt)
--   deriving (Generic,ToJSON,FromJSON)

-- prettyDate (Date (y,m,d)) = (y,intToMonth . read $ fromTxt m,d)
--   where
--     intToMonth n
--       | n < 1 || n > 12 = ""
--       | otherwise =
--           [ "January","February","March"
--           , "April",  "May",     "June"
--           , "July",   "August",  "September"
--           , "October","November","December"
--           ] !! (n - 1)

-- instance Typeable ms => Pure (Renderable Date) ms where
--   render (Render (prettyDate -> (year,month,day))) =
--     Span
--       [ ClassList [ "date" ] ]
--       [ Span [ ClassList [ "month" ] ] [ fromTxt month ]
--       , " "
--       , Span [ ClassList [ "day" ] ] [ fromTxt day ]
--       , ", "
--       , Span [ ClassList [ "year" ] ] [ fromTxt year ]
--       ]

-- dateStyles =
--   is ".date" .>
--     return ()

-- 
-- ---- GITHUBLOGO
-- 
-- ---- GRADIENT
-- 
-- ---- HEADER
-- 
-- ---- INDEX
-- data Index ms = Index
--   { iToggled :: Bool
--   , iSelected :: Maybe Txt
--   , iOnSelect :: Txt -> Ef ms IO ()
--   , iIndex :: [(Txt,[View ms])]
--   }

-- instance Typeable ms => Pure Index ms where
--   render Index {..} =
--     Div
--       [ ClassList [ "index" ]
--       , StyleList
--           [ (opacity,int $ iToggled # 1)
--           , (transition,opacity <<>> sec 0.5 <<>> ease)
--           ]
--       ]
--       [ Div
--           [ ClassList [ "index-inner" ]
--           , StyleList
--               [ (transform, translate (pxs 0 <&>> pxs (not iToggled # 40)))
--               , (transition, transform <<>> sec 0.5 <<>> ease)
--               ]
--           ]
--           $ flip map iIndex $ \(ttl,items) ->
--               Div
--                 [ ClassList [ "index-group" ] ]
--                 [ Div
--                     [ ClassList [ "index-title", (iSelected == Just ttl) # "selected" ]
--                     , onClick (iOnSelect ttl)
--                     ]
--                     (fromTxt $ T.replace "_" " " ttl)
--                 , Div
--                     [ ClassList [ "index-list-outer", (iSelected /= Just ttl) # "collapsed" ] ]
--                     [ Div
--                         [ ClassList [ "index-list-inner" ] ]
--                         items
--                     ]
--                 ]
--       ]

-- indexStyles = do
--   is ".index-title" $ do
--     apply $ do
--       cursor        =: pointer
--       color         =: baseGray
--       fontSize      =: pxs 14
--       fontWeight    =: int 700
--       lineHeight    =: int 2
--       textTransform =: uppercase
--       letterSpacing =: ems 0.08
--       marginBottom  =: ems (0.25)
--       marginLeft    =: ems 1

--     is ".selected" .> do
--       color =: darkGray

--   is ".index-inner" $
--     atMedia "(min-width: 48em)" .>
--       marginLeft =: ems 1

--   is ".index-group" .>
--     marginBottom =: ems 1

--   is ".index-list-outer" $ do
--     apply $ do
--       -- https://stackoverflow.com/questions/3508605/how-can-i-transition-height-0-to-height-auto-using-css
--       display     =: flex
--       overflow    =: hidden

--     is after .> do
--       content =: "''"
--       height  =: pxs 50
--       transition =: height <<>> sec 0.3 <<>> linear
--                <&>> maxHeight <<>> sec 0 <<>> sec 0.3 <<>> linear

--     is ".collapsed" . child ".index-list-inner" .> do
--       marginBottom =: pxs (-2000)
--       transition   =: marginBottom <<>> sec 0.3 <<>> cubicBezier(1,0,1,1)
--                  <&>> visibility <<>> sec 0 <<>> sec 0.3
--                  <&>> maxHeight <<>> sec 0 <<>> sec 0.3
--       visibility   =: hidden
--       maxHeight    =: zero

--     is ".collapsed" . is after .> do
--       height     =: zero
--       transition =: height <<>> sec 0.3 <<>> linear
--       maxHeight  =: pxs 50

--   is ".index-list-inner" $ do
--     apply $ do
--       transition =: marginBottom <<>> sec 0.3 <<>> cubicBezier(0,0,0,1)
--       marginBottom =: zero
--       maxHeight =: pxs 1000000

--   is ".index" $ do
--     atMedia "(max-width: 48em)" .> do
--       padding         =: pxs 40 <<>> pxs 20
--       top             =: pxs 0
--       left            =: pxs 0
--       right           =: pxs 0
--       bottom          =: pxs 0
--       zIndex          =: int 2
--       marginTop       =: pxs 40
--       height          =: calc(vhs 100 <<->> pxs 40)
--       position        =: fixed
--       overflowY       =: scroll
--       backgroundColor =: pureWhite 96

--     atMedia "(min-width: 48em)" .> do
--       paddingTop          =: pxs 10
--       position            =: fixed
--       right               =: zero
--       top                 =: zero
--       width               =: pxs 250
--       marginTop           =: pxs 60
--       minHeight           =: calc(vhs 100 <<->> pxs 60)
--       height              =: per 100
--       backgroundColor     =: pureWhite 96
--       borderLeft          =: pxs 1 <<>> solid <<>> lightGray
--       overflowY       =: scroll
--       important $ opacity =: one

-- 
-- 
-- ---- MARKDOWN
-- instance Typeable ms => Pure (Renderable Markdown) ms where
--   render (Render (Markdown md)) =
--     Div [ ClassList [ "md" ] ]
--       (witnesses md)

-- highlight :: Int -> Int -> CSS ()
-- highlight (toTxt -> blk) (toTxt -> ln) =
--   void . has ("#cb" <> blk <> "-" <> ln) .> do
--     display         =: block
--     backgroundColor =: selection
--     margin          =: zero <<>> ems (-2) <<>> ems (-1.3) <<>> ems (-2)
--     paddingLeft     =: ems 2
--     paddingRight    =: ems 2

-- 
-- ---- TOGGLE
-- data Toggle ms = Toggle
--   { toggled  :: Bool
--   , onToggle :: Ef ms IO ()
--   }

-- instance Typeable ms => Pure Toggle ms where
--   render Toggle {..} =
--     let icon b i =
--           Div
--             [ ClassList [ "material-icons", "toggle-icon" ]
--             , StyleList
--                 [ (opacity,int $ b # 1)
--                 , (transitionDelay,sec $ b # 0.125)
--                 ]
--             ]
--             i
--         -- confusing
--         more = "expand_less"
--         less = "expand_more"

--     in
--       Div
--         [ ClassList [ "toggle" ]
--         , onClick onToggle
--         ]
--         [ Div
--             [ ClassList [ "toggle-inner" ] ]
--             [ icon (not toggled) more
--             , icon toggled less
--             ]
--         ]

-- toggleStyles = do

--     is ".toggle" $ do
--       apply $ do
--         width           =: pxs 60
--         height          =: pxs 60
--         borderRadius    =: per 50
--         boxShadow       =: zero <<>> pxs 1 <<>> "1.5px" <<>> zero <<>> rgba(0,0,0,0.12) <&>>
--                            zero <<>> pxs 1 <<>> pxs 1   <<>> zero <<>> rgba(0,0,0,0.24)
--         border          =: pxs 1 <<>> solid <<>> rgba(255,255,255,0.2)
--         fontWeight      =: int 500
--         position        =: fixed
--         right           =: pxs 20
--         bottom          =: pxs 30
--         backgroundColor =: darkLavender
--         textAlign       =: center
--         zIndex          =: int 3

--       atMedia "(min-width: 48em)" .> do
--         display =: none

--     is ".toggle-inner" $ do
--       apply $ do
--         width    =: per 100
--         height   =: per 100
--         display  =: inlineBlock
--         position =: relative
--         top      =: pxs 6

--     is ".toggle-icon" . is ".material-icons" .> do
--       transition =: opacity <<>> sec 0.25 <<>> "easeInOutCirc"
--       position =: absolute
--       top      =: zero
--       left     =: zero
--       color    =: baseGreen
--       height   =: per 100
--       width    =: per 100
--       fontSize =: pxs 48

-- 
-- ---- WAVE
