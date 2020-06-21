{-# language DeriveAnyClass #-}
module Components.Nav where

import qualified App

import Components.Icons hiding (Model,Msg)
import Data.Route
import Styles.Colors
import Styles.Responsive
import Styles.Themes

import Shared
import Shared.Blog
import Shared.Package
import Shared.Tutorial as Tutorial
import Shared.Types (Episode,Rendered)

import Pure.Elm hiding (Command,Open,target,name,link,url,menu,delay,wait,Left,Right,green,blue,black,gray,lavender,touch)
import Pure.Elm.Application (link,goto,location,url,session)
import Pure.Data.Styles (delay)
import Pure.Data.SVG
import Pure.Maybe
import Pure.Random (newSeed,shuffle)

import Control.Concurrent.Async (wait)
import Control.Monad
import Data.Function (on)
import Data.Maybe (isJust,isNothing)
import Data.List as List (reverse,cycle,take,filter,sortBy)

import Prelude hiding (min,max)

data Menu = AboutMenu | PackagesMenu | TutorialsMenu
  deriving (Eq,Ord,Enum,Show)

target :: Menu -> Route
target AboutMenu = PageR "about"
target PackagesMenu = PackagesR
target TutorialsMenu = TutorialsR 

associated :: Route -> Menu -> Bool
associated (PageR "about") AboutMenu = True
associated (PostR _)       AboutMenu = True
associated PackagesR       PackagesMenu = True
associated (PackageR _)    PackagesMenu = True
associated TutorialsR      TutorialsMenu = True
associated (TutorialR _)   TutorialsMenu = True
associated _               _        = False

menuname :: Menu -> Txt
menuname AboutMenu = "About"
menuname PackagesMenu = "Packages"
menuname TutorialsMenu = "Tutorials"

data Model = Model
  { activeMenu :: Maybe Menu
  , touch :: Bool
  }

data Msg = Clicked Menu | Focused Menu | Touched Menu | Blurred

nav :: App.App => Route -> View
nav = run (App [] [] [] (Model Nothing False) update (view session))

update :: Msg -> Route -> Model -> IO Model
update (Clicked menu) _ mdl = do
  case mdl of
    Model (Just m) True 
      | m == menu -> do
        goto (url id id $ location (target menu))
        pure (Model Nothing False)
      | otherwise -> do
        pure (Model (Just menu) True)
    Model (Just m) False 
      | m == menu -> do
        goto (url id id $ location (target menu))
        pure (Model Nothing False)
      | otherwise -> do
        pure (Model (Just menu) False)
    Model _ True  -> 
      pure (Model (Just menu) True)
    Model _ False -> do
        goto (url id id $ location (target menu))
        pure (Model Nothing False)

update (Focused menu) _ mdl
  | touch mdl = pure mdl
  | otherwise = pure mdl { activeMenu = Just menu }

update (Touched menu) _ mdl = do
  pure mdl { touch = True }

update Blurred _ mdl =
  pure mdl { activeMenu = Nothing }

view :: Elm Msg => App.Session -> Route -> Model -> View
view ses rt mdl =
  Div <| Themed @NavT . OnDoc "click" (const (command Blurred)) |>
    [ links rt
      -- should I aria-label a dynamic secondary navigation?
    , Div <| Themed @SandboxT |>
      [ Div <| Themed @MenusT . vis |> 
        [ menus ses mdl ]
      ]
    ]
  where
    vis | Just _ <- activeMenu mdl = Themed @ActiveT
        | otherwise                = id

links :: Elm Msg => Route -> View
links rt =
  Nav <| Attribute "aria-label" "Primary Navigation" . Themed @LinksT |>
    [ item m | m <- [AboutMenu ..] ]
  where
    item :: Elm Msg => Menu -> View
    item menu =
      A <| Themed @LinkT . focused . clicked . touched |>
        [ txt (menuname menu)
        , lightbar
        ]
      where
        lightbar 
          | associated rt menu = Span <| Themed @LightbarT 
          | otherwise          = Null

        focused = OnMouseEnter (const (command (Focused menu)))
        clicked = OnClickWith intercept (const (command (Clicked menu)))
        touched = OnTouchStart (const (command (Touched menu)))

menus :: Elm Msg => App.Session -> Model -> View
menus ses mdl =
  Div <| Themed @HarnessT . vis |>
    [ Div <| Themed @CardT . vis . activemenu |> 
      [ item m | m <- [AboutMenu ..] ]
    ]
  where
    activemenu 
      | Just m <- activeMenu mdl =
        case m of
          AboutMenu     -> Themed @AboutMenuActiveT
          TutorialsMenu -> Themed @TutorialsMenuActiveT
          PackagesMenu  -> Themed @PackagesMenuActiveT
      | otherwise   = id

    vis 
      | isJust (activeMenu mdl) = Themed @ActiveT 
      | otherwise = id

    item :: Menu -> View
    item m = Div <| Themed @MenuT . vis . menut |> [ menu ses m ]
      where
        menut =
          case m of
            AboutMenu     -> Themed @AboutMenuT
            PackagesMenu  -> Themed @PackagesMenuT
            TutorialsMenu -> Themed @TutorialsMenuT

        vis
          | activeMenu mdl == Nothing = id
          | activeMenu mdl == Just m  = OnMouseLeave (const (command Blurred)) . Themed @ActiveT
          | activeMenu mdl  < Just m  = Themed @MenuRightT
          | activeMenu mdl  > Just m  = Themed @MenuLeftT
          | otherwise                 = id

menu :: App.Session -> Menu -> View
menu ses = \case
  AboutMenu     -> aboutMenu ses
  PackagesMenu  -> documentationMenu ses
  TutorialsMenu -> tutorialsMenu ses

aboutMenu :: App.Session -> View
aboutMenu ses = 
  Div <||>
    [ Section <| Themed @MenuTopT |>
      [ -- Book SVG
        Header <||> 
        [ H1 <||> [ A <| link BlogR |> [ newsIcon, "Blog" ] ]
        , P  <||> [ "Get news and important announcements about Pure.hs" ]
        ]
      , Section <||>
        [ Div <||>
          [ H2 <||> [ "From the blog" ] 
          , -- Pull most recent so it's programatically updated; 
            -- I don't want to be manually updating this
            producing @[Post Rendered] (App.req ses Shared.listPosts () >>= either pure wait) $ consuming $ \ps ->
              Ul <||> 
                [ Li <||>
                  [ A <| link (PostR slug) |>
                    [ txt (toTxt title <> " ❯") ]
                  ]
                | Post {..} <- List.take 4 (List.cycle $ List.sortBy (flip compare) ps)
                ]
          ]
        ]
      ]
    , let 
        list is = Ul <||> [ item svg i l | (svg,i,l) <- is ]
        item svg i l = 
          Li <||> 
            [ A <| either Href link l . either (const (Attribute "target" "_blank")) (const id) l |> 
              [ svg , i ]
            ]
      in       
        Footer <| Themed @MenuBottomT |>
          [ Div <||>
            [ list 
              [ (aboutIcon,"About",Right (PageR "about"))
              , (installIcon,"Install",Right (TutorialR "install"))
              , (playIcon,"Try It Live",Left "http://try.purehs.org")
              , (gitHubLogo_alt,"GitHub",Left "https://github.com/grumply/pure-platform")
              ]
            , list 
              [ (authorsIcon,"Authors",Right AuthorsR)
              , (newsIcon,"Blog",Right BlogR)
              , (discourseIcon,"Forum",Left "http://discourse.purehs.org")
              , (discordIcon,"Chat",Left "https://discord.gg/hVkMsEA")
              ]
            ]
          ]
    ]

documentationMenu :: App.Session -> View
documentationMenu ses =
  Div <||>
    [ Section <| Themed @MenuTopT |> 
      [ Header <||> 
        [ H1 <||> [ A <| link PackagesR |> [ compoundIcon, "Packages" ] ]
        , P  <||> [ "Per-package documentation, blogs and tutorials." ]
        ]
      , Div <||>
        [ H2 <||> [ "Newest Packages" ] 
        , let 
            rq = do
              r <- App.req ses Shared.listPackages () 
              s <- newSeed
              ps <- either pure wait r
              let pub Package {..} = published
              pure (List.take 3 $ List.sortBy (flip compare `on` pub) ps)
          in producing @[Package Rendered] rq $ consuming $ \ps ->
              Ul <||> 
                [ Li <||>
                  [ A <| link (PackageR name) |>
                    [ txt (toTxt name <> " ❯") ]
                  , P <||>
                    [ txt synopsis ]
                  ]
                | Package {..} <- ps
                ]
        ]
      ]
    ]


tutorialsMenu :: App.Session -> View
tutorialsMenu ses =
  Div <||>
    [ Section <| Themed @MenuTopT |> 
      [ Header <||> 
        [ H1 <||> [ A <| link TutorialsR |> [ codeIcon, "Tutorials" ] ]
        , P  <||> [ "Start learning Pure.hs development." ]
        ]
      , Section <||>
        [ Div <||>
          [ H2 <||> [ "Featured Tutorials" ] 
          , let 
              rq = do
                r <- App.req ses Shared.listTutorials () 
                s <- newSeed
                let notEpisode Tutorial {..} = isNothing episode
                ts <- List.filter notEpisode <$> either pure wait r
                pure (List.take 4 $ List.cycle $ shuffle ts s)
            in producing @[Tutorial Rendered] rq $ consuming $ \ts ->
                Ul <||> 
                  [ Li <||>
                    [ A <| link (TutorialR slug) |>
                      [ txt (toTxt title <> " ❯") ]
                    , P <||>
                      [ txt synopsis ]
                    ]
                  | Tutorial {..} <- ts
                  ]
          ]
        ]
      ]
    , let 
        list is = Ul <||> [ item svg i l | (svg,i,l) <- is ]
        item svg i l = 
          Li <||> 
            [ A <| either Href link l . either (const (Attribute "target" "_blank")) (const id) l |> 
              [ svg , i ]
            ]
      in 
        Footer <| Themed @MenuBottomT |> 
          [ Div <||>
            [ list 
              [ (installIcon,"Quickstart",Right (TutorialR "quickstart"))
              , (timerIcon,"5-Minute Series",Right (TutorialR "5-minute"))
              ]
            , list
              [ (lightbulbIcon,"Examples",Right (PageR "examples"))
              , (cookIcon,"Cookbook",Right (PageR "cookbook"))
              ]
            ]
          ]
    ]
data NavT
instance Theme NavT where
  theme c = void $ is c .> do
    display         =: flex
    flex-direction  =: row
    overflow-x      =: auto
    justify-content =: flex-end

data LinksT
instance Theme LinksT where
  theme c = void $ is c .> do
    display         =: flex
    flex-direction  =: row
    overflow-x      =: auto
    justify-content =: flex-end
    cursor          =: pointer

data LinkT
instance Theme LinkT where
  theme c = void $ is c $ do
    apply $ do
      display         =: flex
      flex-direction  =: row
      align-items     =: center
      align-content   =: space-between
      padding-left    =: 10px
      position        =: relative
      font-size       =: 18px
      font-weight     =: 200
      color           =: toTxt base
      text-decoration =: none

    mediumScreens <%> do
      font-size =: 24px

    is hover .> do
      color       =: toTxt base
      text-shadow =* [0,0,5px,toTxt green]

data LightbarT
instance Theme LightbarT where
  theme c = void $ is c .> do
    position         =: absolute
    height           =: 1px
    left             =: 10px
    right            =: 0
    bottom           =: 0
    background-color =: toTxt base

data SandboxT
instance Theme SandboxT where
  theme c = void $ 
    is c $ do
      apply $ do
        position       =: absolute
        top            =: 40px
        overflow       =: hidden
        min-height     =: 320px
        max-height     =: 510px
        width          =: 95vw 
        min-width      =: 400px
        max-width      =: 600px
        pointer-events =: none
        margin-left    =: auto
        margin-right   =: auto
        padding-top    =: 10px

      mediumScreens <%> do
        min-height  =: 435px
        top         =: 60px

data MenusT
instance Theme MenusT where
  theme c = void $ do
    is c $ do
      apply $ do
        position    =: relative
        display     =: inline-block
        float       =: right
        perspective =: 2000px
        opacity     =: 0
        will-change =: opacity

data HarnessT
instance Theme HarnessT where
  theme c = void $ do
    is c $ do
      apply $ do
        position =: absolute
        right    =: 8px

easeInOut = ease-"in"-out

data CardT
instance Theme CardT where
  theme c = void $ 
    is c $ do
      apply $ do
        background-color =: toTxt base
        min-width        =: 300px
        min-height       =: 170px
        max-width        =: 93vw
        max-height       =: 93vh
        border-radius    =: 8px
        box-shadow       =: customBoxShadow 0 5 10 (-2) (rgba(50,50,90,0.75))
        transition       =: elems [transform <<>> 192ms,width <<>> 192ms,height <<>> 192ms]
        transform        =: rotateX((-20)deg)
        transform-origin =: (50%) <<>> (-50)px
        will-change      =: elems [transform,width,height]
        overflow         =: hidden
        line-height      =: 1.2em

      has (tag A) $ do
        apply $ do
          display =: inline-flex
          align-items =: center
          color =: toTxt black
          font-style =: bold

        has (tag Svg) .> do
          width =: 20px
          margin-right =: 4px
          fill =: toTxt black

        is hover $ do
          apply $ do
            color =: toTxt green
          
        is visited $ do
          apply $ do
            color =: toTxt black

        is visited . is hover $ do
          apply $ do
            color =: toTxt green

      is (subtheme @AboutMenuActiveT) $ do
        apply $ do
          width  =: 500px
          height =: 245px

        mediumScreens <%> do
          height =: 365px

      is (subtheme @PackagesMenuActiveT) $ do
        apply $ do
          width  =: 400px
          height =: 270px


      is (subtheme @TutorialsMenuActiveT) $ do
        apply $ do
          width =: 540px
          height =: 185px

        mediumScreens <%> do
          height  =: 410px


data MenuT
instance Theme MenuT where
  theme c = void $ is c .> do
    z-index       =: initial
    position      =: absolute
    height        =: (100%)
    width         =: (100%)
    top           =: 0
    opacity       =: 0
    transition    =: elems [transform <<>> 192ms, opacity <<>> 192ms]
    transform     =: translateX(0)
    will-change   =: elems [transform,opacity,z-index]

data MenuLeftT
instance Theme MenuLeftT where
  theme c = void $ is c .> do
    transform =: translateX((-80)%)

data MenuRightT
instance Theme MenuRightT where
  theme c = void $ is c .> do
    transform =: translateX(80%)

data ActiveT
instance Theme ActiveT where
  theme c = void $ do
    is (subtheme @MenusT) . is c .> do
      opacity    =: 1

    is (subtheme @HarnessT) . is c .> do
      pointer-events =: auto

    is (subtheme @CardT) . is c .> do
      transform  =: rotateX(0deg)

    is (subtheme @MenuT) . is c .> do
      z-index   =: 10
      opacity   =: 1
      transform =: translateX(0) 

data AboutMenuActiveT deriving Theme
data PackagesMenuActiveT deriving Theme
data TutorialsMenuActiveT deriving Theme 

data AboutMenuT deriving Theme
data PackagesMenuT deriving Theme
data TutorialsMenuT deriving Theme

data MenuTopT
instance Theme MenuTopT where
  theme c = void $ do
    is c $ do
      apply $ do
        padding =* [25px,25px,12px,25px]
        background-color =: toTxt base
    
      has (tag Header) $ do
        has (tag H1) $ do
          apply $ do
            margin =: 0
            font-size =: 24px
            color =: toTxt black
          
          has (tag Svg) .> do
            margin-right =: 8px
            width =: 24px

      has (tag P) .> do
        font-size =: 14px
        margin    =* [5px,12px]
        color =: toTxt gray

      has (tag Section) $ do
        apply $ do
          margin-left =: 12px
          display =: none

        mediumScreens <%> do
          display =: block

      has (tag H2) .> do
        font-size =: 16px
        color =: toTxt gray
        margin =* [8px,0px]

      has (tag Ul) $ do
        apply $ do
          font-size =: 14px
          list-style =: none
          padding-left =: 12px
          padding-bottom =: 8px
          margin =: 0

        has (tag Li) $ do
          apply $ do
            margin-top =: 5px

data MenuBottomT
instance Theme MenuBottomT where
  theme c = void $ do
    is c $ do
      apply $ do
        padding-bottom =: 15px
        background-color =: toTxt lavender
        border-bottom-left-radius  =: 8px
        border-bottom-right-radius =: 8px

      child (tag Div) $ do
        apply $ do
          display =: flex
          justify-content =: center
          align-items =: center

      has (tag Ul) $ do
        apply $ do
          display =: inline-block
          list-style =: none
          padding =* [0px, 10px]

        smallScreens <%> do
          padding =* [0px,20px]

        mediumScreens <%> do
          padding =* [0px,60px]

        has (tag Li) $ do
          apply $ do
            margin-top =: 5px

          has (tag A) $ do
            apply $ do
              font-size =: 18px
              color =: toTxt base

            is hover .> do
              color =: toTxt base
              text-shadow =* [0,0,5px,toTxt green]

            is visited .> do
              color =: toTxt base

            is visited . is hover .> do
              color =: toTxt base
              text-shadow =* [0,0,5px,toTxt green]

          has (tag Svg) .> do
            width =: 18px
            fill  =: toTxt base


