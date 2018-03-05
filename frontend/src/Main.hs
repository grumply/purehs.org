{-# LANGUAGE OverloadedStrings, ImplicitParams, TemplateHaskell, PatternSynonyms #-}
module Main where

import qualified Pure.App as App
import Pure.View

import Control.Arrow
import Data.Function
import Data.Maybe
import Data.List

import Lib

main :: IO ()
main =
  let
      ?purehsorgServerIp = "10.0.1.21"
      ?purehsorgServerPort = 8000
      {- END DEPENDENCIES -}
  in let
      key    = "PurehsorgClient"
      build  = return
      prime  =
        void $ delay 1000000 $ do
          unsafePreinit _Blog
          unsafePreinit _Docs
          unsafePreinit (_Tutorial Nothing)
      root   = Nothing
      routes = Routes.routes
      pages  = Main.pages
    in run App {..}

pages HomeR = pure $ page _Head _Home

pages BlogR = pure $ page _Head _Blog
pages (PostR y m d s) = do
  with _Blog (loadPost y m d s)
  with _PostMetaList (setSelectedPost (y,m,d,s))
  pure $ page _Head _Blog

pages DocsR = pure $ page _Head _Docs
pages (DocR n g nm) = do
  with _Docs (loadDocumentation n g nm)
  with _DocMetaList (setSelectedDoc (n,g,nm))
  pure $ page _Head _Docs

pages TutR = pure $ page _Head (_Tutorial Nothing)
pages (TutorialR g c n t) = do
  unsafePreinit (_Tutorial (Just (g,c,n,t)))
  with _TutMetaList (setSelectedTut (g,c,n,t))
  pure $ page _Head (_Tutorial Nothing)

pages ColorsR = pure $ page _Head (Controller "Colors" return (return ()) (Const ()) (const Colors))
{- END PAGES -}

data Routes
  = HomeR
  | ColorsR
  | BlogR
  | PostR Txt Txt Txt Txt
  | DocsR
  | DocR Txt Txt Txt
  | TutR
  | TutorialR Txt Txt Txt Txt
  {- END ROUTES -}
  deriving Eq

routes = do
  path "/colors" (dispatch ColorsR)
  path "/blog/:year/:month/:day/:slug" $ dispatch =<< PostR <$> "year" <*> "month" <*> "day" <*> "slug"
  path "/blog" (dispatch BlogR)
  path "/doc/:group/:module/:function" $ dispatch =<< DocR <$> "group" <*> "module" <*> "function"
  path "/doc" $ dispatch DocsR
  path "/tutorial/:num/:chapter/:group/:name" $ dispatch =<< TutorialR <$> "num" <*> "chapter" <*> "group" <*> "name"
  path "/tutorial" $ dispatch TutR
  path "/reload" $ do
    lift $ message reloadMarkdown ()
    clearLocalStorage
    dispatch HomeR
  {- END ROUTING -}
  dispatch HomeR


-- Head

_Head = Controller {..}
  where
    key    = "Head"
    build  = return
    prime  = return ()
    model  = Const ()
    view _ =
      Head []
        [ scss Main.styles
        , Meta [ Name "viewport", Content "width=device-width, initial-scale=1.0" ] []
        , Link [ Href "https://fonts.googleapis.com/icon?family=Material+Icons", Rel "stylesheet" ] []
        {- END HEAD -}
        ]

styles =
  $(let
      styles = normalize <> staticCSS (do

        is "html" .> do
          boxSizing  =: borderBox

        is "body" .> do
          fontFamily =: "-apple-system, system-ui, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', sans-serif;"
          "text-rendering" =: "optimizeLegibility"
          "-webkit-font-smoothing" =: antialiased
          "-moz-osx-font-smoothing" =: "grayscale"
          backgroundColor =: baseWhite

        )
    in [| styles |]
  )


-- Colors
--------------------------------------------------------------------------------

data Colors = Colors_
  {
  }

pattern Colors = View (Render (Colors_ ))

instance Typeable ms => Pure (Renderable Colors) ms where
  render (Render Colors_ {}) =
    Div [ ClassList [ "colors" ] ]
      [ swatch "dark-blue" darkBlue
      , swatch "base-blue" baseBlue
      , swatch "light-blue" lightBlue

      , swatch "dark-lavender" darkLavender
      , swatch "base-lavender" baseLavender
      , swatch "light-lavender" lightLavender

      , swatch "base-white" baseWhite

      , swatch "light-gray" lightGray
      , swatch "base-gray" baseGray
      , swatch "dark-gray" darkGray

      , swatch "dark-green" darkGreen
      , swatch "base-green" baseGreen
      , swatch "light-green" lightGreen

      , swatch "blue-highlight" blueHighlight
      ]
    where
      swatch colorName c =
        Div [ ClassList [ "swatch" ] ]
          [ fromTxt colorName
          , Div [ ClassList [ "color"], StyleList [(backgroundColor,c)] ] []
          ]

colorsStyles = do

  is ".colors" $ do
    apply $ do
      display =: flex
      flexFlow =: row <<>> "wrap"
      justifyContent =: spaceAround

    has ".swatch" .> do
      padding =: pxs 5
      marginTop =: pxs 10

    has ".color" .> do
      width =: pxs 200
      height =: pxs 200

-- Theme
-- pureBlue, pureLavender, pureWhite, pureGreen :: Double -> Txt
pureBlue p = hsl(215,35.14,p)
pureLavender p = hsl(250,48.59,p)
pureWhite p = hsl(210,2,p)
pureGreen p = hsl(139,68,p)

-- darkBlue, baseBlue, lightBlue :: Txt
darkBlue = pureBlue 20
baseBlue = pureBlue 40
lightBlue = pureBlue 90


-- baseLavender, darkLavender, lightLavender :: Txt
baseLavender = pureLavender 69
darkLavender = pureLavender 49
lightLavender = pureLavender 89

-- baseWhite, baseGray, lightGray, darkGray :: Txt
baseWhite = pureWhite 99
baseGray = pureWhite 70
lightGray = pureWhite 90
darkGray = pureWhite 20

-- baseGreen, lightGreen, darkGreen :: Txt
baseGreen = pureGreen 45
lightGreen = pureGreen 65
darkGreen = pureGreen 25

-- blueHighlight :: Txt
blueHighlight = rgb(58,173,175)

-- -- pureGreen per = hsl(153,68,per)
-- pureGreen per = hsl(140,26,per)
-- pureGray per = hsl(0,0,per)
-- pureWhite per = hsl(60,25,per)
-- pureOrange, pureRed :: Double -> Txt
pureOrange per = hsl(29,90,per)
pureRed per = hsl(6,87,per)

-- baseWhite = pureWhite 99.3
-- baseOrange = pureOrange 68
-- baseRed = pureRed 63

-- darkGreen = pureGreen 40
-- baseGreen = pureGreen 59
-- lightGreen = pureGreen 80
-- brightGreen = pureGreen 90

-- baseGray = pureGray 33

-- dark = hsl(218,15,15)

-- Code highlighting

-- h,s,l :: Int
h         = 220 -- hue
s         = 13 -- saturation
l         = 18 -- brightness

-- mono1, mono2, mono3 :: Txt
mono1     = hsl(h,14,71)
mono2     = hsl(h,09,55)
mono3     = hsl(h,10,40)

-- cyan_, blue_, purple_, green_, red1, red2, orange1, orange2 :: Txt
cyan_     = hsl(187,47,55) -- hue1
blue_     = hsl(207,82,66) -- hue2
purple_   = hsl(286,60,67) -- hue3
green_    = hsl( 95,38,62) -- hue4
red1      = hsl(355,65,65) -- hue5
red2      = hsl(  5,48,51) -- hue5-2
orange1   = hsl( 29,54,61) -- hue6
orange2   = hsl( 39,67,69) -- hue6-2

-- fg, bg, gutter, guide, accent, selection :: Txt
fg        = mono1
bg        = hsl(220,13,18)
gutter    = hsl(h,14,45)
guide     = hsla(h,14,71,0.15)
accent    = hsl(h,100,66)
selection = hsl(220,13,10)


{-
pureGreen per = hsl(140,31,per)
pureGray per = hsl(0,0,per)
pureWhite per = hsl(54,12,per)
purePeach per = hsl(29,59,per)
pureRed per = hsl(6,68,per)

baseGreen = pureGreen 70
baseGray = pureGray 33
baseWhite = pureWhite 96
basePeach = purePeach 97
baseRed = pureRed 95

darkGreen = pureGreen 40
mediumGreen = pureGreen 60
lightGreen = pureGreen 80
brightGreen = pureGreen 90
-}


-- Home

_Home = Controller {..}
  where
    key    = "Home"
    build  = return
    prime  = return ()
    model  = Const ()
    view _ =
      Div
        [ ClassList [ "Home" ] ]
        [ Hdr Nothing
        , Section []
            [ Gradient
            , Wave
            , intro
            ]
        ]
      where
        intro =
          Div [ ClassList [ "intro" ] ]
            [ Div [ ClassList [ "hero" ] ]
                [ H1 [ ClassList [ "tag" ] ]
                    [ "The web from a "
                    , I [] "different angle." ]
                , P [ ClassList [ "hero-description" ] ]
                    [ "Pure is a Haskell-based web stack that focuses on"
                    , Br [] []
                    , "performance, expressiveness, and asynchrony."
                    ]
                , A [ Lref "/doc", ClassList [ "call-to-action", "get-started" ] ] "Get Started"
                , A [ Lref "/tutorial", ClassList [ "call-to-action", "start-tutorial" ] ] "Take Tutorial"
                ]
            ]

homeStyles = do

  is ".Home" $ do
    apply $ do
      minHeight =: calc(vhs 100 <<->> pxs 60)
      backgroundColor =: baseWhite

    atMedia "(max-width: 48em)" .> do
      minHeight =: calc (vhs 100 <<->> pxs 40)

    has ".logo-icon" $ do
      apply $ do
        height =: pxs 120
        marginLeft =: pxs 30

      has ".letter" .> do
        fill =: baseWhite

    has ".tag" .> do
      marginBottom =: pxs 16
      fontWeight =: int 200
      fontSize   =: pxs 40
      textShadow =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

    has ".hero-description" .> do
      fontSize =: pxs 16
      textShadow =: pxs 1 <<>> pxs 1 <<>> hsla(215,35.14,40,0.5)

    has ".header" .> do
      backgroundColor =: initial

    has ".intro" $ do
      apply $ do
        color =: baseWhite
        position =: absolute
        width =: per 100
        height =: per 100
        display =: flex
        alignItems =: center

    has ".hero" $ do
      apply $ do
        position =: relative
        width =: per 100
        margin =: auto <<>> auto
        textAlign =: center
        fontWeight =: int 200

    has ".call-to-action" $ do
      let buttonBoxShadow opacity vOff blur vOff' blur' =
                 zero <<>> pxs vOff  <<>> pxs blur  <<>> hsla(250,48.59,69,opacity)
            <&>> zero <<>> pxs vOff' <<>> pxs blur' <<>> rgba(0,0,0,0.1)

      apply $ do
        display =: inlineBlock
        height =: pxs 40
        lineHeight =: pxs 40
        padding =: zero <<>> pxs 14
        boxShadow =: buttonBoxShadow 0.13 4 6 1 3
        borderRadius =: pxs 5
        fontSize     =: pxs 15
        fontWeight   =: int 600
        textTransform =: uppercase
        letterSpacing =: ems 0.025
        textDecoration =: none
        transition =: "all" <<>> sec 0.15 <<>> ease

      is hovered .> do
        transform =: translateY(pxs (-1))
        boxShadow =: buttonBoxShadow 0.11 7 14 3 6

      is active .> do
        transform =: translateY(pxs 1)
        boxShadow =: buttonBoxShadow 0.13 4 6 1 3

    has ".get-started" .> do
      backgroundColor =: baseGreen
      color           =: baseWhite
      marginRight     =: pxs 16

    has ".start-tutorial" .> do
      backgroundColor =: baseWhite
      color           =: baseLavender


-- Blog

data BlogState ms = BlogState
  { bsPost :: Maybe (Txt,Txt,Txt,Txt)
  , bsPostsToggled :: Bool
  } deriving Typeable

_Blog = Controller {..}
  where
    key = "Blog"
    build = return
    prime = void $
      remote getFeaturedPost () $ traverse_ $ \mp -> do
        for mp $ \p -> liftIO $ do
          with (_Post (pmPath $ pMeta p) mp) (return ())
          with _PostMetaList (setSelectedPost $ pmPath $ pMeta p)
          putLocalItemIfNotExists (buildPostPath $ pmPath (pMeta p)) p
        modifyModel $ \BlogState {..} -> BlogState { bsPost = fmap (pmPath . pMeta) mp, .. }
    model = BlogState Nothing False
    view BlogState {..} =
      Div
        [ ClassList [ "Blog" ] ]
        [ Hdr (Just BlogActive)
        , may (mvc Article [ ClassList [ "main" ] ] . flip _Post Nothing) bsPost
        , mvc Div [] _PostMetaList
        ]

loadPost y m d s = do
  mres <- demandMaybe =<< getLocalItem (buildPostPath (y,m,d,s))
  with (_Post (y,m,d,s) (snd <$> join mres)) (return ())
  modifyModel $ \BlogState {..} -> BlogState { bsPost = Just (y,m,d,s), .. }

blogStyles = do
  is ".Blog" $ do
    apply $
      backgroundColor =: baseWhite

    has ".header" .> do
      position  =: fixed

    has ".main" $ do
      apply $ do
        minHeight   =: calc(vhs 100 <<->> pxs 60)
        maxWidth    =: per 100
        padding     =: ems 1

      atMedia "(max-width: 48em)" .> do
        height      =: calc(vhs 100 <<->> pxs 40)

      atMedia "(min-width: 48em)" .> do
        marginLeft  =: ems 1
        marginRight =: calc(pxs 250)

  blogFeaturesStyles


---- Post

data PostState ms = PostState
  { psPost :: Maybe Post
  } deriving Typeable

_Post (y,m,d,t) mp = Controller {..}
  where
    key = "Post_" <> fromTxt y <> fromTxt m <> fromTxt d <> fromTxt t
    build = return
    prime = loadPost (y,m,d,t)
    model = PostState mp
    view PostState {..} =
      flip may psPost $ \Post {..} ->
        Div [ ClassList [ "Post" ] ]
          [ css (for_ (pmHighlights pMeta) (uncurry highlight))
          , H1 [ ClassList [ "title" ] ] [ fromTxt (pmTitle pMeta) ]
          , Span [ ClassList [ "byline" ] ]
              [ View $ Render $ pmPathToDate $ pmPath pMeta
              , " by "
              , Span [ ClassList [ "author" ] ] [ fromTxt (pmAuthor pMeta) ]
              ]
          , Div [ ClassList [ "md" ] ]
              (witnesses pContent)
          ]

pmPathToDate (y,m,d,_) = Date (y,m,d)

setPost mp = modifyModel $ \PostState {..} -> PostState { psPost = mp, .. }

loadPost ymdt = do
  PostState {..} <- getModel
  unless (isJust psPost) $
    void $ remote getPost ymdt (traverse_ setPost)

postStyles = do

  is ".Post" $ do

    apply $ do
      marginTop   =: pxs 45
      maxWidth    =: pxs 1260
      marginLeft  =: auto
      marginRight =: auto

    atMedia "(min-width: 48em)" .> do
      marginTop =: pxs 60

    has "h1" .> do
      marginBottom =: zero
      fontSize     =: pxs 40
      fontWeight   =: int 700

    has ".byline" .> do
      return ()


---- PostMetaList

data PostMetaListState ms = PostMetaListState
  { pmlsMetas :: [PostMeta]
  , pmlsSelected :: (Txt,Txt,Txt,Txt)
  , pmlsToggled :: Bool
  , pmlsCurrent :: Maybe Txt
  } deriving Typeable

_PostMetaList = Controller {..}
  where
    key = "PostMetaList"
    build = return
    prime = void loadPostMetas
    model = PostMetaListState def def def (Just "Recent Posts")
    view PostMetaListState {..} =
      Div
        [ ClassList [ "PostMetaList" ] ]
        [ View $ Index pmlsToggled pmlsCurrent onSel
            [("Recent Posts",flip map pmlsMetas $ \pm ->
               View $ PostMetaListItem pm (pmlsSelected == pmPath pm) $ do
                 togglePostMetaList (const False)
                 setSelectedPost (pmPath pm)
             )]
        , View (Toggle pmlsToggled (togglePostMetaList not))
        ]

      where
        onSel txt = modifyModel $ \PostMetaListState {..} ->
          let pmlsCurrent' = if pmlsCurrent == Just txt then Nothing else Just txt
          in PostMetaListState { pmlsCurrent = pmlsCurrent', .. }

togglePostMetaList f =
  modifyModel $ \PostMetaListState {..} ->
    PostMetaListState { pmlsToggled = f pmlsToggled, .. }

setSelectedPost pm =
  modifyModel $ \PostMetaListState {..} ->
    PostMetaListState { pmlsSelected = pm, .. }

loadPostMetas =
  remote getPostMetas () $ \pms ->
    modifyModel $ \PostMetaListState {..} ->
      PostMetaListState { pmlsMetas = fromMaybe [] pms, .. }

postMetaListStyles = do

  is ".PostMetaList" .>
    return ()


---- PostMetaListItem

data PostMetaListItem ms = PostMetaListItem
  { pmliPostMeta :: PostMeta
  , pmliSelected :: Bool
  , pmliOnToggle :: Ef ms IO ()
  }

instance Typeable ms => Pure PostMetaListItem ms where
  render PostMetaListItem {..} = let PostMeta {..} = pmliPostMeta in
    Div
      [ ClassList
          [ "post-meta-list-item-meta"
          , pmliSelected # "post-meta-list-item-selected"
          ]
      ]
      [ A
          [ ClassList [ "post-meta-list-item-link" ]
          , Lref (buildPostPath pmPath)
          , onClick pmliOnToggle
          ]
          [ Div
              [ ClassList [ "post-meta-list-item-title" ] ]
              [ fromTxt pmTitle ]
          ]
      ]

buildPostPath (y,m,d,t) = "/blog/" <> y <> "/" <> m <> "/" <> d <> "/" <> t

postMetaListItemStyles = do
  is ".post-meta-list-item-selected" $ do
    apply $ do
      borderLeft =: pxs 3 <<>> solid <<>> baseGreen

    has ".post-meta-list-item-title" .> do
      color =: darkGray
      fontWeight =: bold

  is ".post-meta-list-item-meta" .> do
    marginLeft =: zero
    marginTop =: zero
    paddingLeft =: calc(ems 0.5 <<->> pxs 3)

  is ".post-meta-list-item-link" .> do
    textDecoration =: none
    width =: pxs 250
    whiteSpace =: nowrap
    overflow =: hidden
    textOverflow =: ellipsis
    textDecoration =: none

  is ".post-meta-list-item-title" $ do
    apply $ do
      color =: baseGray
      margin =: zero
      marginLeft =: ems 0.5
      fontSize =: pxs 14

    is hovered .>
      color =: darkGray


-- Docs

data DocsState ms = DocsState
  { dsDoc :: Maybe (Txt,Txt,Txt)
  , dsHome :: Markdown
  } deriving Typeable

_Docs = Controller {..}
  where
    key = "Docs"
    build = return
    prime = do
      void $ remote getFeaturedDocumentation () $ traverse_ $ \md -> do
        for md $ \d -> liftIO $ do
          unsafePreinit (_Doc (dmPath $ dMeta d) md)
          asDocList <- usingController _DocMetaList Preinit
          asDocList (setSelectedDoc $ dmPath $ dMeta d)
          asDocList (setCurrentDocGroup $ documentGroup $ dMeta d)
          putLocalItemIfNotExists (buildDocPath $ dmPath (dMeta d)) d
        DocsState {..} <- getModel
        putModel DocsState { dsDoc = fmap (dmPath . dMeta) md, .. }
    model = DocsState Nothing (Markdown [])
    view DocsState {..} =
      Div
        [ ClassList [ "Docs" ] ]
        [ Hdr (Just DocsActive)
        , maybe
            (Article [ ClassList [ "main", "home" ] ] [ View $ Render dsHome ])
            (mvc Article [ ClassList [ "main" ] ] . flip _Doc Nothing)
            dsDoc
        , mvc Div [] _DocMetaList
        ]

loadDocumentation n g nm = do
  mres <- demandMaybe =<< getLocalItem (buildDocPath (n,g,nm))
  liftIO $ print $ isJust $ join mres
  with (_Doc (n,g,nm) (fmap snd $ join mres)) (return ())
  modifyModel $ \DocsState {..} -> DocsState { dsDoc = Just (n,g,nm), .. }

docsStyles = do

  is ".Docs" $ do
    apply $ do
      backgroundColor =: baseWhite

    has ".main" $ do
      apply $ do
        marginTop   =: pxs 45
        maxWidth    =: pxs 1260
        marginLeft  =: auto
        marginRight =: auto

      atMedia "(min-width: 48em)" .> do
        marginTop =: pxs 60

      apply $ do
        minHeight   =: calc(vhs 100 <<->> pxs 60)
        maxWidth    =: per 100
        padding     =: ems 1

      atMedia "(max-width: 48em)" .> do
        height      =: calc(vhs 100 <<->> pxs 40)

      atMedia "(min-width: 48em)" .> do
        marginLeft  =: ems 1
        marginRight =: calc(pxs 250)

      has ".home" $ do
        apply $ do
          marginTop   =: pxs 45
          maxWidth    =: pxs 1260
          marginLeft  =: auto
          marginRight =: auto

        atMedia "(min-width: 48em)" .> do
          marginTop =: pxs 60

        has "h1" .> do
          marginBottom =: zero
          fontSize     =: pxs 40
          fontWeight   =: int 700

        has ".byline" .> do
          return ()


---- Doc

data DocState ms = DocState
  { dsMayDoc :: Maybe Doc
  } deriving Typeable

_Doc (n,g,nm) md = Controller {..}
  where
    key = "Doc_" <> fromTxt n <> "-" <> fromTxt g <> "-" <> fromTxt nm
    build = return
    prime = loadDoc (n,g,nm)
    model = DocState md
    view DocState {..} =
      flip may dsMayDoc $ \Doc {..} ->
        Div [ ClassList [ "Doc" ] ]
          [ css (for_ (dmHighlights dMeta) (uncurry highlight))
          , H1 [ ClassList [ "title" ] ] [ fromTxt (dmTitle dMeta) ]
          , Div [ ClassList [ "md" ] ]
              (witnesses dContent)
          ]

activateDoc md = do
  for md $ \d ->
    with _DocMetaList (setCurrentDocGroup (documentGroup (dMeta d)))
  modifyModel $ \DocState {..} -> DocState { dsMayDoc = md, .. }

loadDoc (n,g,nm) = do
  DocState {..} <- getModel
  unless (isJust dsMayDoc) $
    void $ remote getDoc (n,g,nm) (traverse_ activateDoc)

docStyles = do

  is ".Doc" $ do

    apply $ do
      marginTop   =: pxs 45
      maxWidth    =: pxs 1260
      marginLeft  =: auto
      marginRight =: auto

    atMedia "(min-width: 48em)" .> do
      marginTop =: pxs 60

    has "h1" .> do
      marginBottom =: zero
      fontSize     =: pxs 40
      fontWeight   =: int 700


---- DocMetaList

data DocMetaListState ms = DocMetaListState
  { dmlsMetas :: [DocMeta]
  , dmlsSelected :: (Txt,Txt,Txt)
  , dmlsToggled :: Bool
  , dmlsCurrent :: Maybe Txt
  } deriving Typeable

_DocMetaList = Controller {..}
  where
    key = "DocMetaList"
    build = return
    prime = void loadDocMetas
    model = DocMetaListState def def def def
    view DocMetaListState {..} =
      Div
        [ ClassList [ "DocsList" ] ]
        [ View $ Index dmlsToggled dmlsCurrent onSel $ (fmap . fmap) (fmap renderItem) (groupAndSortDocs dmlsMetas)
        , View (Toggle dmlsToggled (toggleDocMetaList not))
        ]
      where
        onSel txt = modifyModel $ \DocMetaListState {..} ->
          let dmlsCurrent' = if dmlsCurrent == Just txt then Nothing else Just txt
          in DocMetaListState { dmlsCurrent = dmlsCurrent', .. }


        renderItem dm =
          View $ DocMetaListItem dm (dmlsSelected == dmPath dm) $ do
            toggleDocMetaList (const False)
            setCurrentDocGroup (documentGroup dm)
            setSelectedDoc (dmPath dm)

        groupAndSortDocs =
          map (documentGroup . head &&& id) .
          map (sortOn documentName) .
          groupBy ((==) `on` documentGroup) .
          sortOn documentNum

toggleDocMetaList f =
  modifyModel $ \DocMetaListState {..} ->
    DocMetaListState { dmlsToggled = f dmlsToggled, .. }

setSelectedDoc dm =
  modifyModel $ \DocMetaListState {..} ->
    DocMetaListState { dmlsSelected = dm, .. }

setCurrentDocGroup dg =
  modifyModel $ \DocMetaListState {..} ->
    DocMetaListState { dmlsCurrent = Just dg, .. }

loadDocMetas =
  remote getDocMetas () $ \dms ->
    modifyModel $ \DocMetaListState {..} ->
      DocMetaListState { dmlsMetas = fromMaybe [] dms, .. }

docMetaListStyles = do

  is ".DocMetaList" .> do
    return ()


---- DocMetaListItem

data DocMetaListItem ms = DocMetaListItem
  { dmliDocMeta  :: DocMeta
  , dmliSelected :: Bool
  , dmliOnToggle :: Ef ms IO ()
  }

instance Typeable ms => Pure DocMetaListItem ms where
  render DocMetaListItem {..} = let DocMeta {..} = dmliDocMeta in
    Div
    [ ClassList
        [ "doc-meta-list-item-meta"
        , dmliSelected # "doc-meta-list-item-selected"
        ]
    ]
    [ A
      [ ClassList [ "doc-meta-list-item-link" ]
      , Lref (buildDocPath dmPath)
      , onClick dmliOnToggle
      ]
      [ Div
        [ ClassList [ "doc-meta-list-item-title" ] ]
        [ fromTxt dmTitle ]
      ]
    ]

buildDocPath (n,g,nm) = "/doc/" <> n <> "/" <> g <> "/" <> nm

docMetaListItemStyles = do
  is ".doc-meta-list-item-selected" $ do
    apply $ do
      borderLeft =: pxs 3 <<>> solid <<>> baseGreen

    has ".doc-meta-list-item-title" .> do
      color =: darkGray
      fontWeight =: bold

  is ".doc-meta-list-item-meta" .> do
    marginLeft  =: zero
    marginTop   =: zero
    paddingLeft =: calc(ems 0.5 <<->> pxs 3)

  is ".doc-meta-list-item-link" .> do
    textDecoration =: none
    width          =: pxs 250
    whiteSpace     =: nowrap
    overflow       =: hidden
    textOverflow   =: ellipsis
    textDecoration =: none

  is ".doc-meta-list-item-title" $ do
    apply $ do
      color      =: baseGray
      margin     =: zero
      marginLeft =: ems 0.5
      fontSize   =: pxs 14

    is hovered .>
      color =: darkGray


-- Tutorial

data TutorialState ms = TutorialState
  { tsTutorial :: Maybe (Txt,Txt,Txt,Txt)
  } deriving Typeable

_Tutorial mtut = Controller {..}
  where
    key = "Tutorial"
    build base = return (state (Map.empty :: HashMap Txt Tutorial) *:* base)
    prime =
      case mtut of
        Just gcnt -> void $ sync $ loadTutorial gcnt
        Nothing -> do
          void $ remote getFeaturedTutorial () $ traverse_ $ \mt -> do
            for mt $ \t -> liftIO $ do
              unsafePreinit (_Tut (tmPath $ tMeta t))
              asTutList <- usingController _TutMetaList Preinit
              asTutList (setSelectedTut $ tmPath $ tMeta t)
              asTutList (setCurrentTutGroup $ tutorialGroup $ tMeta t)
              putLocalItemIfNotExists (buildTutPath (tmPath $ tMeta t)) t
            modifyModel $ \TutorialState {..} ->
              TutorialState { tsTutorial = fmap (tmPath . tMeta) mt, .. }
    model = TutorialState mtut
    view TutorialState {..} =
      Div
        [ ClassList [ "Tutorial" ] ]
        [ Hdr (Just TutorialActive)
        , may (mvc Article [ ClassList [ "main" ] ] . _Tut) tsTutorial
        , mvc Div [] _TutMetaList
        ]

loadTut gcnt@(buildTutPath -> tutPath) = do
  tuts <- liftAsync get
  case Map.lookup tutPath tuts of
    Just t -> return $ Just t
    Nothing -> do
      mres <- Async $ getLocalItem tutPath
      case mres of
        Just (_,t) -> do
          unsafePreinit (_Tut gcnt)
          with (_Tut gcnt) (setTutorial t)
          liftAsync $ put $ Map.insert tutPath t tuts
          return (Just t)
        Nothing -> do
          mt <- Async $ withPromise $ \pr -> void $ remote getTutorial gcnt (void . fulfill pr)
          case join mt of
            Nothing -> return Nothing
            Just t  -> do
              putLocalItemIfNotExists (buildTutPath (tmPath $ tMeta t)) t
              unsafePreinit (_Tut gcnt)
              with (_Tut gcnt) (setTutorial t)
              liftAsync $ put $ Map.insert tutPath t tuts
              return (Just t)

loadTutorial gcnt = do
  mtut <- loadTut gcnt
  case mtut of
    Nothing -> return ()
    Just (tMeta -> tm) -> do
      void $ with _TutMetaList $ do
        setSelectedTut (tmPath tm)
        setCurrentTutGroup (tutorialGroup tm)


tutorialStyles = do

  is ".Tutorial" $ do
    apply $ do
      backgroundColor =: baseWhite

    has ".main" $ do
      apply $ do
        minHeight   =: calc(vhs 100 <<->> pxs 60)
        maxWidth    =: per 100
        padding     =: ems 1

      atMedia "(max-width: 48em)" .> do
        height      =: calc(vhs 100 <<->> pxs 40)

      atMedia "(min-width: 48em)" .> do
        marginLeft  =: ems 1
        marginRight =: calc(pxs 250)


---- Tut

data TutState ms = TutState
  { tsMayTut :: Maybe Tutorial
  } deriving Typeable

_Tut (g,c,n,t) = Controller {..}
  where
    key = "Tut_" <> fromTxt g <> "-" <> fromTxt n <> "-" <> fromTxt c <> "-" <> fromTxt t
    build = return
    prime = return ()
    model = TutState Nothing
    view TutState {..} =
      flip may tsMayTut $ \Tutorial {..} ->
        Div [ ClassList [ "Tut" ] ]
          [ css (for_ (tmHighlights tMeta) (uncurry highlight))
          , H1 [ ClassList [ "title" ] ] [ fromTxt (tmTitle tMeta) ]
          , Span [ ClassList [ "byline" ] ]
              [ "By "
              , Span [ ClassList [ "author" ] ] [ fromTxt (tmAuthor tMeta) ]
              ]
          , Div [ ClassList [ "md" ] ]
              (witnesses tContent)
          ]

setTutorial tut = modifyModel $ \TutState {..} -> TutState { tsMayTut = Just tut, .. }

-- activateTut mt = do
--   for mt $ \t ->
--     with _TutMetaList (setCurrentTutGroup (tutorialGroup (tMeta t)))
--   modifyModel $ \TutState {..} -> TutState { tsMayTut = mt, .. }

-- loadTut gcnt = do
--   liftIO $ print $ "activating: " ++ show gcnt
--   TutState {..} <- getModel
--   unless (isJust tsMayTut) $ do
--     mres <- demandMaybe =<< getLocalItem (buildTutPath gcnt)
--     case join mres of
--       Just (_,t) -> activateTut (Just t)
--       Nothing -> void $ remote getTutorial gcnt (traverse_ activateTut)

tutStyles = do

  is ".Tut" $ do
    apply $ do
      marginTop   =: pxs 45
      maxWidth    =: pxs 1260
      marginLeft  =: auto
      marginRight =: auto

    atMedia "(min-width: 48em)" .> do
      marginTop =: pxs 60

    has "h1" .> do
      marginBottom =: zero
      fontSize     =: pxs 40
      fontWeight   =: int 700

    has ".byline" .> do
      return ()


---- TutMetaList

data TutMetaListState ms = TutMetaListState
  { tmlsMetas :: [TutorialMeta]
  , tmlsSelected :: (Txt,Txt,Txt,Txt)
  , tmlsToggled :: Bool
  , tmlsCurrent :: Maybe Txt
  } deriving Typeable

_TutMetaList = Controller {..}
  where
    key = "TutMetaList"
    build = return
    prime = void loadTutMetas
    model = TutMetaListState def def def def
    view TutMetaListState {..} =
        Div
          [ ClassList [ "TutMetaList" ] ]
          [ View $ Index tmlsToggled tmlsCurrent onSel $ (fmap . fmap) (fmap renderItem) (groupAndSortTuts tmlsMetas)
          , View (Toggle tmlsToggled (toggleTutMetaList not))
          ]
      where
        onSel txt = modifyModel $ \TutMetaListState {..} ->
          let tmlsCurrent' = if tmlsCurrent == Just txt then Nothing else Just txt
          in TutMetaListState { tmlsCurrent = tmlsCurrent', .. }

        renderItem tm =
          View $ TutMetaListItem tm (tmlsSelected == tmPath tm) $ do
            toggleTutMetaList (const False)
            setCurrentTutGroup (tutorialGroup tm)
            setSelectedTut (tmPath tm)

        groupAndSortTuts =
          map (tutorialGroup . head &&& id) .
          map (sortOn tutorialCh) .
          groupBy ((==) `on` tutorialGroup) .
          sortOn tutorialNum

toggleTutMetaList f =
  modifyModel $ \TutMetaListState {..} ->
    TutMetaListState { tmlsToggled = f tmlsToggled, .. }

setSelectedTut tm =
  modifyModel $ \TutMetaListState {..} ->
    TutMetaListState { tmlsSelected = tm, .. }

setCurrentTutGroup tg =
  modifyModel $ \TutMetaListState {..} ->
    TutMetaListState { tmlsCurrent = Just tg, .. }

loadTutMetas =
  remote getTutorialMetas () $ \tms -> do
    modifyModel $ \TutMetaListState {..} ->
      TutMetaListState { tmlsMetas = fromMaybe [] tms, .. }

tutMetaListStyles = do

  is ".TutMetaList" .> do
    return ()


---- TutMetaListItem

data TutMetaListItem ms = TutMetaListItem
  { tmliTutorialMeta :: TutorialMeta
  , tmliSelected     :: Bool
  , tmliOnToggle     :: Ef ms IO ()
  }

instance Typeable ms => Pure TutMetaListItem ms where
  render TutMetaListItem {..} = let TutorialMeta {..} = tmliTutorialMeta in
    Div
      [ ClassList
          [ "tut-meta-list-item-meta"
          , tmliSelected # "tut-meta-list-item-selected"
          ]
      ]
      [ A
          [ ClassList [ "tut-meta-list-item-link" ]
          , Lref (buildTutPath tmPath)
          , onClick tmliOnToggle
          ]
          [ Div
              [ ClassList [ "tut-meta-list-item-title" ] ]
              [ fromTxt tmTitle ]
          ]
      ]

buildTutPath (group,chapter,name,title) = "/tutorial/" <> group <> "/" <> chapter <> "/" <> name <> "/" <> title

tutMetaListItemStyles = do

  is ".tut-meta-list-item-selected" $ do
    apply $ do
      borderLeft =: pxs 3 <<>> solid <<>> baseGreen
      paddingLeft =: calc(ems 0.5 <<->> pxs 3)

    has ".tut-meta-list-item-title" .> do
      color =: darkGray
      fontWeight =: bold

  is ".tut-meta-list-item-meta" .> do
    marginLeft =: zero
    marginTop =: zero
    paddingLeft =: ems 0.5

  is ".tut-meta-list-item-link" .> do
    textDecoration =: none
    width =: pxs 250
    whiteSpace =: nowrap
    overflow =: hidden
    textOverflow =: ellipsis
    textDecoration =: none

  is ".tut-meta-list-item-title" $ do
    apply $ do
      color =: baseGray
      margin =: zero
      marginLeft =: ems 0.5
      fontSize =: pxs 14

    is hovered .>
      color =: darkGray


-- Features

---- Date

newtype Date = Date (Txt,Txt,Txt)
  deriving (Generic,ToJSON,FromJSON)

prettyDate (Date (y,m,d)) = (y,intToMonth . read $ fromTxt m,d)
  where
    intToMonth n
      | n < 1 || n > 12 = ""
      | otherwise =
          [ "January","February","March"
          , "April",  "May",     "June"
          , "July",   "August",  "September"
          , "October","November","December"
          ] !! (n - 1)

instance Typeable ms => Pure (Renderable Date) ms where
  render (Render (prettyDate -> (year,month,day))) =
    Span
      [ ClassList [ "date" ] ]
      [ Span [ ClassList [ "month" ] ] [ fromTxt month ]
      , " "
      , Span [ ClassList [ "day" ] ] [ fromTxt day ]
      , ", "
      , Span [ ClassList [ "year" ] ] [ fromTxt year ]
      ]

dateStyles =
  is ".date" .>
    return ()


---- GithubLogo

data Github = Github { githubLink :: Txt }

pattern GithubLogo link = View (Render (Github link))

instance Typeable ms => Pure (Renderable Github) ms where
  render (Render (Github link)) =
    Pure.View.A
      [ ClassList [ "github-link" ]
      , Href link
      , Target "_blank"
      ]
      [ github ]

githubLogoStyles = do

  is ".github-icon" .> do
    fill =: white

github =
  Svg
    [ ViewBox "0 0 24 24"
    , ClassList [ "github-icon" ]
    ]
    [ Path
        [ D "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12" ]
        [ ]
    ]


---- Gradient

pattern Gradient = Div [ ClassList [ "gradient" ] ] []

gradientStyles = do

  is ".gradient" .> do
    position =: absolute
    width =: per 100
    height =: calc(per 100 <<->> pxs 5)
    overflow =: hidden
    transformOrigin =: zero
    background =: linearGradient(deg 150
                                  <&>> darkLavender <<>> per 15
                                  <&>> blueHighlight <<>> per 70
                                  <&>> lightGreen <<>> per 95
                                )


---- Header

data ActivePage = BlogActive | DocsActive | TutorialActive
  deriving (Eq,Enum,Show)

data Header = Hdr_ (Maybe ActivePage)

pattern Hdr active = View (Render (Hdr_ active))

instance Typeable ms => Pure (Renderable Header) ms where
  render (Render (Hdr_ active)) =
    Header
      [ ClassList [ "header" ] ]
      [ Div
          [ ClassList [ "bar" ] ]
          [ Logo False True
          , Nav [] $
              flip map (enumFrom BlogActive) $ \l ->
                let link uri nm = A [ ClassList [ "link" ], Lref uri ]
                                    [ nm
                                    , (active == Just l) # Span [ ClassList [ "highlight" ] ] []
                                    ]
                in case l of
                    BlogActive      -> link "/blog" "Blog"
                    DocsActive      -> link "/doc" "Docs"
                    TutorialActive -> link "/tutorial" "Tutorial"
          , GithubLogo "https://github.com/grumply/pure"
          ]
      ]

headerStyles = do

  is ".header" $ do
    apply $ do
      zIndex          =: int 4
      position        =: absolute
      width           =: per 100
      top             =: zero
      left            =: zero
      backgroundColor =: darkLavender
      display         =: block


    has ".bar" $ do
      apply $ do
        paddingLeft   =: pxs 20
        paddingRight  =: pxs 20
        marginLeft    =: auto
        marginRight   =: auto
        height        =: pxs 60
        display       =: flex
        alignItems    =: center
        justifyContent =: spaceBetween
        flexDirection =: row

      atMedia "(max-width: 48em)" .> do
        height =: pxs 40

      has "nav" $ do
        apply $ do
          width         =: per 60
          display       =: flex
          flexDirection =: row
          alignItems    =: center
          height        =: per 100
          overflowX     =: auto

        atMedia "(max-width: 48em)" .> do
          width        =: per 80
          alignItems   =: start

      has ".link" . or has ".logo-link" . or has ".github-link" .> do
        display       =: flex
        flexDirection =: row
        alignItems    =: center
        alignContent  =: spaceBetween
        boxSizing     =: borderBox

      has ".github-link" $
        atMedia "(max-width: 48em)" .> do
          display     =: none

      has ".link" $ do
        apply $ do
          height         =: per 100
          paddingLeft    =: pxs 20
          paddingRight   =: pxs 20
          position       =: relative
          fontSize       =: pxs 24
          fontWeight     =: int 200
          color          =: white
          textDecoration =: none

        atMedia "(max-width: 48em)" $ do
          apply $ do
            paddingLeft  =: pxs 10
            paddingRight =: pxs 10
            fontSize     =: pxs 18

        has ".highlight" $ do
          apply $ do
            position        =: absolute
            height          =: pxs 3
            left            =: zero
            right           =: zero
            bottom          =: pxs 0
            backgroundColor =: blueHighlight

      has ".logo-link" $ do
        apply $ do
          marginRight =: ems 0.75

        has ".logo" $ do
          apply $ do
            margin =: pxs 5 <<>> auto
            height =: pxs 45

          atMedia "(max-width: 48em)" .> do
            height =: pxs 35

      has ".github-link" $ do
        apply $ width =: per (100 / 6)

        has ".github-icon" .> do
          marginLeft =: auto
          width  =: pxs 30

    atMedia "(min-width: 780px)"  $ has ".bar" .> width =: per 90

    atMedia "(min-width: 1340px)" $ has ".bar" .> maxWidth =: pxs 1260


---- Index

data Index ms = Index
  { iToggled :: Bool
  , iSelected :: Maybe Txt
  , iOnSelect :: Txt -> Ef ms IO ()
  , iIndex :: [(Txt,[View ms])]
  }

instance Typeable ms => Pure Index ms where
  render Index {..} =
    Div
      [ ClassList [ "index" ]
      , StyleList
          [ (opacity,int $ iToggled # 1)
          , (transition,opacity <<>> sec 0.5 <<>> ease)
          ]
      ]
      [ Div
          [ ClassList [ "index-inner" ]
          , StyleList
              [ (transform, translate (pxs 0 <&>> pxs (not iToggled # 40)))
              , (transition, transform <<>> sec 0.5 <<>> ease)
              ]
          ]
          $ flip map iIndex $ \(ttl,items) ->
              Div
                [ ClassList [ "index-group" ] ]
                [ Div
                    [ ClassList [ "index-title", (iSelected == Just ttl) # "selected" ]
                    , onClick (iOnSelect ttl)
                    ]
                    (fromTxt $ T.replace "_" " " ttl)
                , Div
                    [ ClassList [ "index-list-outer", (iSelected /= Just ttl) # "collapsed" ] ]
                    [ Div
                        [ ClassList [ "index-list-inner" ] ]
                        items
                    ]
                ]
      ]

indexStyles = do
  is ".index-title" $ do
    apply $ do
      cursor        =: pointer
      color         =: baseGray
      fontSize      =: pxs 14
      fontWeight    =: int 700
      lineHeight    =: int 2
      textTransform =: uppercase
      letterSpacing =: ems 0.08
      marginBottom  =: ems (0.25)
      marginLeft    =: ems 1

    is ".selected" .> do
      color =: darkGray

  is ".index-inner" $
    atMedia "(min-width: 48em)" .>
      marginLeft =: ems 1

  is ".index-group" .>
    marginBottom =: ems 1

  is ".index-list-outer" $ do
    apply $ do
      -- https://stackoverflow.com/questions/3508605/how-can-i-transition-height-0-to-height-auto-using-css
      display     =: flex
      overflow    =: hidden

    is after .> do
      content =: "''"
      height  =: pxs 50
      transition =: height <<>> sec 0.3 <<>> linear
               <&>> maxHeight <<>> sec 0 <<>> sec 0.3 <<>> linear

    is ".collapsed" . child ".index-list-inner" .> do
      marginBottom =: pxs (-2000)
      transition   =: marginBottom <<>> sec 0.3 <<>> cubicBezier(1,0,1,1)
                 <&>> visibility <<>> sec 0 <<>> sec 0.3
                 <&>> maxHeight <<>> sec 0 <<>> sec 0.3
      visibility   =: hidden
      maxHeight    =: zero

    is ".collapsed" . is after .> do
      height     =: zero
      transition =: height <<>> sec 0.3 <<>> linear
      maxHeight  =: pxs 50

  is ".index-list-inner" $ do
    apply $ do
      transition =: marginBottom <<>> sec 0.3 <<>> cubicBezier(0,0,0,1)
      marginBottom =: zero
      maxHeight =: pxs 1000000

  is ".index" $ do
    atMedia "(max-width: 48em)" .> do
      padding         =: pxs 40 <<>> pxs 20
      top             =: pxs 0
      left            =: pxs 0
      right           =: pxs 0
      bottom          =: pxs 0
      zIndex          =: int 2
      marginTop       =: pxs 40
      height          =: calc(vhs 100 <<->> pxs 40)
      position        =: fixed
      overflowY       =: scroll
      backgroundColor =: pureWhite 96

    atMedia "(min-width: 48em)" .> do
      paddingTop          =: pxs 10
      position            =: fixed
      right               =: zero
      top                 =: zero
      width               =: pxs 250
      marginTop           =: pxs 60
      minHeight           =: calc(vhs 100 <<->> pxs 60)
      height              =: per 100
      backgroundColor     =: pureWhite 96
      borderLeft          =: pxs 1 <<>> solid <<>> lightGray
      overflowY       =: scroll
      important $ opacity =: one


---- Logo

data Logo = Logo_
  { lSimple :: Bool
  , lLinked :: Bool
  } deriving (Generic,Default)

pattern Logo lSimple lLinked = View (Render (Logo_ {..}))

instance Typeable ms => Pure (Renderable Logo) ms where
  render (Render (Logo_ {..})) = let svg = lSimple ? simpleLogo $ logo in
    Div
      [ ClassList [ "logo" ] ]
      [ lLinked
          ? Pure.View.A
              [ ClassList [ "logo-link" ]
              , Lref "/"
              ]
              [ svg ]
          $ svg
      ]

logoStyles = do

  is ".logo" $ do

    has ".bracket" .> do
      fill =: baseWhite

    has ".letter" .> do
      fill =: baseGreen

    has ".ext" .> do
      fill =: baseWhite

    has ".period" .> do
      fill =: baseWhite

  is ".logo-icon" $ do

    has ".letter" .> do
      fill =: blueHighlight


simpleLogo =
  Svg
    [ ViewBox "0 0 50.23 50.48"
    , ClassList [ "logo-icon" ]
    ]
    [ G [ ] [ p ] ]
  where

    path cls d = Path [ ClassList [ cls ], D d ] [ ]

    p = path "letter" "M41.14,18.42c0,7-6.6,10.74-10.74,10.74a1.32,1.32,0,0,1-1.33-1.3s0-.06,0-.08a1.25,1.25,0,0,1,1.26-1.2c3.42-.18,8.16-3.18,8.16-8.16,0-4.68-3.66-8-8.7-8a14.23,14.23,0,0,0-7.32,1.92c2.44,4.64,3.44,10.82,3.44,16.82,0,11-3.42,17.28-8.58,17.28-4.42,0-7.42-4-7.42-12.72C9.92,26.1,12.68,17,19,11.64a9.19,9.19,0,0,0-7.12-3.72,1.23,1.23,0,0,1-1.32-1.14V6.72A1.36,1.36,0,0,1,11.86,5.4,11.33,11.33,0,0,1,21,10.2,17.45,17.45,0,0,1,29.8,7.92C36.52,7.92,41.14,12.42,41.14,18.42ZM20.26,13.86c-5.58,5-7.68,13.86-7.68,19.86,0,5.22,1.5,10.38,4.86,10.38,3.12,0,5.7-5.58,5.82-14.4C23.32,24.06,22.48,18.06,20.26,13.86Z"

logo =
  Svg
    [ ViewBox "0 0 157.38 50.58"
    , ClassList [ "logo" ]
    ]
    [ G [ ] [ open, pure, dot, hs, close ] ]
  where

    pure = G [ ClassList [ "pure" ] ] [ p, u, r, e ]
    hs   = G [ ClassList [ "ext"  ] ] [ h, s ]

    path cls d = Path [ ClassList [ cls ], D d ] [ ]

    bracket = path "bracket"
    letter  = path "letter"
    period  = path "period"
    ext     = path "ext"

    open = bracket "M13.86,47.94a1.32,1.32,0,0,1,1.2,1.38,1.2,1.2,0,0,1-1.44,1.14H9.18c-2.46,0-5.34-.18-6.24-1C1.2,47.76.36,43.5.06,28.2,0,26.76,0,25.08,0,23.4,0,14.58.3,4,1.14,1.62A1.58,1.58,0,0,1,2.64.42H4.92A51.69,51.69,0,0,0,12.54,0h.18A1.26,1.26,0,0,1,14,1.32a1.23,1.23,0,0,1-1.14,1.2,61.55,61.55,0,0,1-9.42.42C3,5.64,2.7,13.92,2.7,23c0,12.48.42,23.22,1.8,24.12,1,.6,2.94.78,4.74.78Z"

    p = letter "M47.22,13.5c0,7-6.6,10.74-10.74,10.74a1.31,1.31,0,0,1-1.32-1.38,1.26,1.26,0,0,1,1.26-1.2c3.42-.18,8.16-3.18,8.16-8.16,0-4.68-3.66-8-8.7-8a14.33,14.33,0,0,0-7.32,1.92C31,12.06,32,18.24,32,24.24c0,11-3.42,17.28-8.58,17.28C19,41.52,16,37.5,16,28.8c0-7.62,2.76-16.68,9.06-22.08A9.21,9.21,0,0,0,17.94,3a1.23,1.23,0,0,1-1.32-1.2A1.37,1.37,0,0,1,17.94.48a11.34,11.34,0,0,1,9.12,4.8A17.56,17.56,0,0,1,35.88,3C42.6,3,47.22,7.5,47.22,13.5ZM26.34,8.94c-5.58,5-7.68,13.86-7.68,19.86,0,5.22,1.5,10.38,4.86,10.38,3.12,0,5.7-5.58,5.82-14.4C29.4,19.14,28.56,13.14,26.34,8.94Z"

    u = letter "M51.24,22.68a50.4,50.4,0,0,0-1.44,10.5c0,2.76.54,5.64,2.52,5.64,3.84,0,7.32-7.8,8.64-17.28a1.38,1.38,0,0,1,1.56-1.14c.6,0,1.08.78,1,1.5s-.42,2.88-.84,4.92a55.87,55.87,0,0,0-1,8.1,5.79,5.79,0,0,0,1.2,4.14,1.36,1.36,0,0,1,.3.9,1.33,1.33,0,0,1-1.32,1.32c-1.26,0-2.76-2.52-2.7-5.88-1.56,3.72-4.14,6-6.78,6-2.16,0-5.16-1.38-5.16-8.22A59,59,0,0,1,48.48,22.5c.12-.54.3-1.5,1.38-1.5a1.4,1.4,0,0,1,1.38,1.38Z"

    r = letter "M70.08,40.14a1.3,1.3,0,0,1-1.32,1.14A1.23,1.23,0,0,1,67.56,40c0-.42,1.68-10.26,1.68-12.24,0-2.64-.54-3.66-1.32-4.44a1.14,1.14,0,0,1-.36-1A1.32,1.32,0,0,1,68.82,21c1.14,0,2.82,2,2.7,5a8,8,0,0,1,7.56-5.76,1.36,1.36,0,0,1,1.44,1.32,1.22,1.22,0,0,1-1.26,1.2c-6.24.36-7.44,7.68-8.82,15.36Z"

    e = letter "M93.54,25c0-1.2-.84-2-2.4-2-3,0-7.26,3.84-7.26,10.26,0,2,.66,5.7,5,5.7a5.5,5.5,0,0,0,4.2-2,1.17,1.17,0,0,1,1-.48,1.25,1.25,0,0,1,1.2,1.38c0,1.14-2.88,3.54-6.54,3.54-4.56,0-7.5-3.06-7.5-8.16,0-7.56,5.28-12.84,10-12.84,3.6,0,5,2.22,5,4.62,0,4.74-5,7.08-8.52,7.08a1.24,1.24,0,0,1-1.32-1.26,1.37,1.37,0,0,1,1.32-1.32C90.54,29.46,93.54,27.72,93.54,25Z"

    dot = period "M100,38.1A1.45,1.45,0,0,1,101.7,37a1.39,1.39,0,0,1,1.26,1.5l-.3,2a1.41,1.41,0,0,1-1.5,1.08,1.31,1.31,0,0,1-1.44-1.5Z"

    h = ext "M108.24,40a25.88,25.88,0,0,1-.42-4.92,86.3,86.3,0,0,1,2-16.8,13.57,13.57,0,0,1-1.62.06c-1.08,0-2.34.06-2.34-1.32a1.15,1.15,0,0,1,1.26-1.2h1.14a13.5,13.5,0,0,0,2.28-.18c1.86-5.94,5.58-12.72,10.2-12.72a3.74,3.74,0,0,1,4.08,4c0,4.86-6.18,9.36-12.18,10.86a104.09,104.09,0,0,0-2.22,12.18c1.8-6,5.46-9.66,8.7-9.66,4.74,0,4.8,6.3,4.8,8.88a42.75,42.75,0,0,1-1.44,10.44,1.34,1.34,0,0,1-1.26,1,1.38,1.38,0,0,1-1.38-1.32c0-.12.06-.24.06-.36a43,43,0,0,0,1.32-9.78c0-4.56-.78-6.12-2.28-6.12-3.9,0-9,10-8.16,16.68A1.18,1.18,0,0,1,109.5,41,1.29,1.29,0,0,1,108.24,40Zm5.22-25c4.68-1.62,8.7-5.1,8.7-8a1.38,1.38,0,0,0-1.5-1.56C118.5,5.34,115.32,9.3,113.46,14.94Z"

    s = ext "M134,33.9a1.05,1.05,0,0,1-1.2,1.14,4.54,4.54,0,0,0-1-.18c-1.14,0-1.44.9-1.44,1.74,0,1.44,1.74,2.88,3.9,2.88s4.44-1.26,4.44-4c0-1.44-.72-3.12-3.42-4.56-1.92-1-4.38-2.64-4.38-5.64,0-2.46,2-5,5.7-5s5,2.58,5,3.6a1.36,1.36,0,0,1-1.38,1.38,1.21,1.21,0,0,1-1.08-.78,2.82,2.82,0,0,0-2.58-1.92,2.72,2.72,0,0,0-3,2.76c0,1.14.54,1.86,2.94,3.3,2.94,1.74,4.92,3.48,4.92,6.54,0,3.72-2.64,6.48-7.2,6.48-3.42,0-6.54-2-6.54-5a4.15,4.15,0,0,1,4.14-4.32C132.9,32.34,134,32.7,134,33.9Z"

    close = bracket "M143.76,47.52c2.7.18,6.06.48,8.1.48a9,9,0,0,0,2.1-.18,67.37,67.37,0,0,1-.36-7.8c0-5.64.36-8.64.6-15.84.24-5.76.48-12.3.48-16.62a34.28,34.28,0,0,0-.18-4.38,3.45,3.45,0,0,0-2-.42,37.33,37.33,0,0,0-4.92.54,23.37,23.37,0,0,1-3,.36,1.3,1.3,0,0,1-1.26-1.38,1.18,1.18,0,0,1,1.26-1.2c.54,0,1.5-.18,2.58-.3A37.51,37.51,0,0,1,152.4.24c5,0,5,2,5,7.5,0,5.28-1.08,26.58-1.08,32.4a49.54,49.54,0,0,0,.42,7.74v.24c0,2.1-2.58,2.46-5,2.46-1.68,0-3.9-.18-6.72-.42-1.86-.18-2.7-.24-2.7-1.5a1.19,1.19,0,0,1,1.32-1.14Z"


---- Markdown

instance Typeable ms => Pure (Renderable Markdown) ms where
  render (Render (Markdown md)) =
    Div [ ClassList [ "md" ] ]
      (witnesses md)

highlight :: Int -> Int -> CSS ()
highlight (toTxt -> blk) (toTxt -> ln) =
  void . has ("#cb" <> blk <> "-" <> ln) .> do
    display         =: block
    backgroundColor =: selection
    margin          =: zero <<>> ems (-2) <<>> ems (-1.3) <<>> ems (-2)
    paddingLeft     =: ems 2
    paddingRight    =: ems 2

markdownStyles =
  is ".md" $ do

    apply $ do
      marginTop  =: pxs 16

    child "p" . is ":first-child" .> do
      lineHeight =: dec 1.7
      fontSize   =: pxs 18
      fontWeight =: int 300
      color      =: rgba(81,81,81,0.7)

    -- child "p" . is ":first-child" .> do
    --   marginTop  =: zero

    has "h2" .> do
      margin     =: zero
      fontSize   =: pxs 32

    has "h3" .> do
      margin     =: zero
      fontSize   =: pxs 28

    has "h2" . is ":first-child" .> do
      marginTop  =: zero
      paddingTop =: zero

    has "h2" . next "h3" .> do
      paddingTop =: pxs 16

    has "p" .> do
      marginTop  =: pxs 16
      fontSize   =: pxs 16

    has "p" . has "code" .> do
      fontFamily       =: "'Operator Mono', source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
      wordBreak        =: breakWord
      padding          =: zero <<>> pxs 4
      background       =: pureOrange 88

    has "blockquote" $ do
      apply $ do
        backgroundColor =: pureOrange 93
        borderLeft      =: pxs 10 <<>> solid <<>> pureRed 82
        padding         =: pxs 16 <<>> pxs 0 <<>> pxs 16 <<>> pxs 16
        margin          =: pxs 20 <<>> pxs 0 <<>> pxs 20 <<>> pxs (-16)

      atMedia "(max-width: 48em)" .> do
        marginRight =: pxs (-16)

      has "h1" .> do
        margin =: pxs 8
        fontSize =: pxs 20

      has "p" .> do
        marginTop =: zero

    has "p" . has "a" $ do
      apply $ do
        background       =: pureGreen 85
        borderBottom     =: pxs 1 <<>> solid <<>> baseGreen
        color            =: selection
        textDecoration   =: none

      is hovered .> do
        background       =: lightGreen
        borderBottom     =: pxs 1 <<>> solid <<>> darkGreen

    has "pre" . is ".sourceCode" .> do
      marginTop        =: pxs 32
      marginLeft       =: pxs (-16)
      marginRight      =: pxs (-16)
      marginBottom     =: pxs 16
      paddingLeft      =: pxs 16
      paddingRight     =: pxs 16
      fontFamily       =: "'Operator Mono', source-code-pro, Menlo, Monaco, Consolas, 'Courier New', monospace"
      fontWeight       =: int 300
      backgroundColor  =: bg
      color            =: fg
      overflow         =: auto

    has "pre" . is ".sourceCode" $ do
      atMedia "(min-width: 48em)" .> do
        borderRadius     =: pxs 10
        -- marginLeft       =: pxs 16
        -- marginRight      =: pxs 16

    has "code" . is ".sourceCode" .> do
      fontFamily       =: inherit
      margin           =: pxs 16
      fontSize         =: pxs 16
      lineHeight       =: dec 1.3
      display          =: block
      paddingBottom    =: pxs 2

    has ".sourceLine" .> do
      whiteSpace       =: preWrap
      display          =: inlineBlock
      lineHeight       =: dec 1.5
      width            =: per 100

    has "code" $ do
      has "span" $ do

        is ".co" .> do { color =: mono3; fontStyle =: italic }                         -- Comment
        is ".dt" .> color =: orange2                                                   -- DataType
        is ".kw" .> do { color =: purple_ }                                            -- Keyword
        is ".cf" .> do { color =: purple_ }                                            -- ControlFlow
        is ".op" .> color =: mono1                                                     -- Operator
        is ".ot" .> color =: blue_                                                     -- Other
        is ".sc" .> color =: blue_                                                     -- SpecialChar
        is ".ss" .> color =: blue_                                                     -- SpecialString
        is ".vs" .> color =: blue_                                                     -- VerbatimString
        is ".cn" .> color =: orange1                                                   -- Constant
        is ".dv" .> color =: orange1                                                   -- DecVal
        is ".bn" .> color =: orange1                                                   -- BaseN
        is ".fl" .> color =: orange1                                                   -- Float
        is ".ch" .> color =: orange1                                                   -- Char
        is ".st" .> color =: green_                                                    -- String
        is ".va" .> color =: red1                                                      -- Variable
        is ".fu" .> color =: cyan_                                                     -- Function
        is ".al" .> do { color =: red2; fontWeight =: bold }                           -- Alert
        is ".er" .> do { color =: red2; fontWeight =: bold }                           -- Error
        is ".wa" .> do { color =: red1; fontWeight =: bold; fontStyle =: italic }      -- Warning
        is ".im" .> color =: purple_                                                   -- Import
        is ".bu" .> color =: purple_                                                   -- BuiltIn
        is ".ex" .> color =: purple_                                                   -- Extension
        is ".do" .> do { color =: mono3; fontStyle =: italic }                         -- Documentation
        is ".an" .> do { color =: purple_; fontWeight =: bold; fontStyle =: italic }   -- Annotation
        is ".cv" .> do { color =: mono3; fontWeight =: bold; fontStyle =: italic }     -- CommentVar
        is ".in" .> do { color =: mono3; fontWeight =: bold; fontStyle =: italic }     -- Information


---- Toggle

data Toggle ms = Toggle
  { toggled  :: Bool
  , onToggle :: Ef ms IO ()
  }

instance Typeable ms => Pure Toggle ms where
  render Toggle {..} =
    let icon b i =
          Div
            [ ClassList [ "material-icons", "toggle-icon" ]
            , StyleList
                [ (opacity,int $ b # 1)
                , (transitionDelay,sec $ b # 0.125)
                ]
            ]
            i
        -- confusing
        more = "expand_less"
        less = "expand_more"

    in
      Div
        [ ClassList [ "toggle" ]
        , onClick onToggle
        ]
        [ Div
            [ ClassList [ "toggle-inner" ] ]
            [ icon (not toggled) more
            , icon toggled less
            ]
        ]

toggleStyles = do

    is ".toggle" $ do
      apply $ do
        width           =: pxs 60
        height          =: pxs 60
        borderRadius    =: per 50
        boxShadow       =: zero <<>> pxs 1 <<>> "1.5px" <<>> zero <<>> rgba(0,0,0,0.12) <&>>
                           zero <<>> pxs 1 <<>> pxs 1   <<>> zero <<>> rgba(0,0,0,0.24)
        border          =: pxs 1 <<>> solid <<>> rgba(255,255,255,0.2)
        fontWeight      =: int 500
        position        =: fixed
        right           =: pxs 20
        bottom          =: pxs 30
        backgroundColor =: darkLavender
        textAlign       =: center
        zIndex          =: int 3

      atMedia "(min-width: 48em)" .> do
        display =: none

    is ".toggle-inner" $ do
      apply $ do
        width    =: per 100
        height   =: per 100
        display  =: inlineBlock
        position =: relative
        top      =: pxs 6

    is ".toggle-icon" . is ".material-icons" .> do
      transition =: opacity <<>> sec 0.25 <<>> "easeInOutCirc"
      position =: absolute
      top      =: zero
      left     =: zero
      color    =: baseGreen
      height   =: per 100
      width    =: per 100
      fontSize =: pxs 48


---- Wave

data Wave = Wave_
  {
  }

pattern Wave = View (Render (Wave_ ))

instance Typeable ms => Pure (Renderable Wave) ms where
  render (Render Wave_ {}) =
    Svg [ ClassList [ "wave" ], ViewBox "0 0 1200 28" ]
      [ Path
          [ Fill baseWhite
          , Transform (translate(int 600 <&>> int 13) <<>> scale(negativeOne <&>> negativeOne) <<>> translate(neg (int 600) <&>> neg (int 15)))
          , D "M0,0 L1200,0 L1200,7 C1200,7 1154,17 1024,18 C895,19 653,8 412,8 C171,8 0,21 0,21 L0,0 Z"
          ]
          []
      ]

waveStyles = do

  is ".wave" $ do
    apply $ do
      position =: absolute
      bottom =: zero
      width =: per 100
