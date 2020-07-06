{-# language DeriveAnyClass #-}
module Components.Markdown where

import Components.Editor as Editor
import Components.CopyToClipboard
import Components.Preload
import Data.Render
import Styles.Colors
import Styles.Fonts
import Styles.Responsive
import Styles.Themes

import Shared.Types
import Shared.Utils (fnv64)

import Pure.Elm.Application hiding (render,link,selection,red,orange,green,black,brightness,gray,blue)
import Pure.Data.Txt as Txt (lines,dropWhileEnd,dropWhile,count)

import GHC.Exts (IsList(..))

import Prelude hiding (break,max,even)
import Data.Char
import Data.List as List (lookup,length,foldl')

import Pure.WebSocket hiding (none)
import System.IO.Unsafe

instance Render Rendered where
  render (Rendered md) = 
    Section <| Themed @ContentT |>
      [ Div <| Themed @MarkdownT |> 
        [ processTry (processCopyable (processLinksWith options v))
        | v <- toList md
        ]
      ]
    where
      options = def { preloader = Just pre }
      pre ref = OnTouchStart (\_ -> preload (Internal ref)) 
              . OnMouseOver (\_ -> preload (Internal ref))

processTry :: View -> View
processTry (Children (texts -> t) (Attributes as Pre)) 
  | Just _ <- List.lookup "data-try" as 
  = let n = realToFrac (Txt.count "\n" t) + 1
        m = realToFrac (Txt.count "\n\n" t)
        y = 20 * n + 46
    in Div <| Height (pxs y) . Themed @EditorT |>
         [ Editor.editor (Textarea <||> [ txt t ]) ]
processTry v = setChildren (fmap processTry (getChildren v)) v

texts :: [View] -> Txt
texts = Txt.dropWhile isSpace . List.foldl' append ""
  where
    append t (TextView _ a) = t <> "\n" <> Txt.dropWhileEnd isSpace a
    append t _ = t

instance Render (Excerpt Rendered) where
  render (Excerpt md) = render md

instance Render (Changes Rendered) where
  render (Changes md) = render md

data ContentT
instance Theme ContentT where
  theme c = void $ do
    is (subtheme @PlaceholderT) . has c .> do
      filter_ =: blur(8px)

    is c $ do
      apply $ do
        width  =: (100%)
        margin =* [0,auto]

      mediumScreens <%> do
        width =: 520px

      largeScreens <%> do
        width =: 700px

data EditorT deriving Theme
data MarkdownT
instance Theme MarkdownT where
  theme c = void $ is c $ do
    apply $ do
      -- for inheriting
      color =: toTxt black

    has ".hide" .>
      display =: none

    has ".footnotes" .> do
      margin-top =: 1em

    has (subtheme @EditorT) $ do
      apply $ 
        width  =: (100%)

      has (tag A) $ do
        apply $ do
          color           =: toTxt gray
          text-decoration =: none
          border          =: none
          background      =: white

        is hover .> do
          color =: toTxt green
          background      =: white
          border          =: none

        is hover . is visited .> do
          color =: toTxt green
          background      =: white
          border          =: none

        is visited .> do
          color =: toTxt gray
          background      =: white
          border          =: none

    has (tag Figure) $ do
      apply $ do
        margin     =* [30px,0]
        text-align =: center

      largeScreens <%> do
        margin =* [30px,(-80)px]
        
      has (tag Img) .> do
        max-width =: (100%)
        height    =: auto

      has (tag Figcaption) .> do
        text-align  =: center
        margin-top  =: 8px
        color       =: toTxt black
        font-family =: defaultFont
        font-size   =: 16px

    id .> do
      width =: (100%)

    has (tag P) .> do
      line-height =: 28px
      font-size   =: 16px
      font-weight =: 400
      color       =: toTxt black
      margin      =: 16px

    has (tag A) $ do
      apply $ do
        background      =: toTxt (faded green)
        border-bottom   =* [1px,solid,toTxt green]
        color           =: toTxt black
        text-decoration =: none

      is hover .> do
        background    =: toTxt green

      is hover . is visited .> do
        background    =: toTxt green

      is visited .> do
        background    =: toTxt (faded green)

    has (tag Li) .> do
      line-height =: 20px
      font-size   =: 18px
      font-weight =: 400
      color       =: toTxt base { brightness = 25 }
      margin      =: 16px

    has (tag H1) .> do
      font-family =: titleFont
      font-size   =: 48px
      margin      =: 0

    has (tag H2) .> do
      font-family =: titleFont
      font-size =: 32px

    has (tag H2) . has (tag A) .> important $ do
      background =: none
      border-bottom =: none

    has (tag H2) . has (tag A) . is hover .> important $ do
      color =: toTxt green
      background =: none
      border-bottom =: none

    has (tag H2) . has (tag A) . is visited . is hover .> important $ do
      color =: toTxt green
      background =: none
      border-bottom =: none

    has (tag H3) .> do
      font-family =: titleFont
      color       =: toTxt base { brightness = 45 }
      font-size   =: 24px
      padding-top =: 1em

    has (tag H3) . has (tag A) .> important $ do
      background =: none
      border-bottom =: none

    has (tag H3) . has (tag A) . is hover .> important $ do
      color =: toTxt green
      background =: none
      border-bottom =: none

    has (tag H3) . has (tag A) . is visited . is hover .> important $ do
      color =: toTxt green
      background =: none
      border-bottom =: none

    has (tag P) . has (tag Code) .> do
      font-family =: defaultMonoFont
      word-break  =: break-word
      padding     =* [3px,4px,2px,4px]
      background  =: toTxt (faded orange)

    has (tag Li) . has (tag Code) .> do
      font-family =: defaultMonoFont
      word-break  =: break-word
      padding     =* [3px,4px,2px,4px]
      background  =: toTxt (faded orange)

    has (tag Blockquote) $ do
      apply $ do
        border-left      =* [5px,solid,toTxt black]
        padding          =* [16px,0px,16px,8px]
        margin           =* [20px,0px,20px,(-16)px]

      child (tag P) .> do
        margin-bottom =: 0

      atMedia "(max-width: 48em)" $ do
        apply $
          margin-right =: (-16)px

        has ".sourceCode" . has (tag Pre) .> do
          margin-right =: 0

      atMedia "(min-width: 48em)" .> do
        margin-left  =: 16px
        margin-right =: 16px

      has (tag H2) .> do
        font-family =: titleFont
        margin    =: 8px
        font-size =: 20px

      has (tag H3) .> do
        font-family =: titleFont
        border-top =: none
        margin     =: 8px
        font-size  =: 18px

      has (tag P) .> do
        font-style =: italic
        font-weight =: 400
        font-family =: serifFont
        font-size   =: 18px
        color =: toTxt gray
        margin-top =: 0

    has ".warn" $ do
      apply $ do
        background-color =: toTxt (faded orange)
        border-left      =* [10px,solid,toTxt red]
        padding          =* [16px,0px,16px,16px]
        margin           =* [20px,0px,20px,(-16)px]

      child (tag P) .> do
        margin-bottom =: 0

      atMedia "(max-width: 48em)" $ do
        apply $
          margin-right =: (-16)px

        has ".sourceCode" . has (tag Pre) .> do
          margin-right =: 0

      atMedia "(min-width: 48em)" .> do
        margin-left  =: 16px
        margin-right =: 16px

      has (tag H2) .> do
        padding-top =: 0
        font-family =: titleFont
        margin    =: 8px
        font-size =: 20px

      has (tag H3) .> do
        padding-top =: 0
        font-family =: titleFont
        border-top =: none
        margin     =: 8px
        font-size  =: 18px

      has (tag P) .> do
        margin-top =: 0

    has ".info" $ do
      apply $ do
        background-color =: toTxt (faded blue)
        border-left      =* [10px,solid,toTxt blue]
        padding          =* [16px,0px,16px,16px]
        margin           =* [20px,0px,20px,(-16)px]

      child (tag P) .> do
        margin-bottom =: 0

      atMedia "(max-width: 48em)" $ do
        apply $
          margin-right =: (-16)px

        has ".sourceCode" . has (tag Pre) .> do
          margin-right =: 0

      atMedia "(min-width: 48em)" .> do
        margin-left  =: 16px
        margin-right =: 16px

      has (tag H2) .> do
        padding-top =: 0
        font-family =: titleFont
        margin    =: 8px
        font-size =: 20px

      has (tag H3) .> do
        padding-top =: 0
        font-family =: titleFont
        border-top =: none
        margin     =: 8px
        font-size  =: 18px

      has (tag P) .> do
        margin-top =: 0

    has ".more" $
      has (tag A) $ do
        apply $ do
          display       =: block
          text-align    =: right
          margin-right  =: 16px
          margin-top    =: 30px
          color         =: toTxt black
          border-bottom =: none
          background    =: none
          font-size     =: 18px

        is hover .> do
          color =: toTxt green
          background =: none

        is visited .> do
          color =: toTxt black
          background =: none

        is visited . is hover .> do
          color =: toTxt green
          background =: none

    has ".prev" $ do
      apply $ do
        position =: absolute
        bottom =: 20px
        left =: 8vw

      has (tag A) $ do
        apply $ do
          display       =: block
          float         =: left
          margin-left   =: (-16)px
          margin-top    =: 8px
          color         =: toTxt black
          border-bottom =: none
          background    =: none
          font-size     =: 18px

        is hover .> do
          color =: toTxt green
          background =: none

        is visited .> do
          color =: toTxt black
          background =: none

        is visited . is hover .> do
          color =: toTxt green
          background =: none

    has ".next" $ do
      apply $ do
        position =: absolute
        bottom =: 20px
        right =: 8vw

      has (tag A) $ do
        apply $ do
          display       =: block
          float         =: right
          margin-right  =: (-16)px
          margin-top    =: 8px
          color         =: toTxt black
          border-bottom =: none
          background    =: none
          font-size     =: 18px

        is hover .> do
          color =: toTxt green
          background =: none

        is visited .> do
          color =: toTxt black
          background =: none

        is visited . is hover .> do
          color =: toTxt green
          background =: none

    has (tag Pre) . is ".sourceCode" .> do
      margin-left           =: (-16)px
      margin-right          =: (-16)px
      font-family           =: defaultMonoFont
      font-weight           =: 400
      webkit-font-smoothing =: auto
      background-color      =: toTxt black
      color                 =: mono1
      overflow              =: auto
      overflow              =: moz-"scrollbars"-none
      microsoft-overflow-style =: none

    has (tag Pre) . is ".sourceCode" . is "::-webkit-scrollbar" .> important $ do
      width  =: 0
      height =: 0

    has (tag Pre) . is ".sourceCode" $
      atMedia "(min-width: 48em)" .> do
        border-radius =: 10px
        margin-left   =: 16px
        margin-right  =: 16px

    has (tag Code) . is ".sourceCode" .> do
      font-family    =: inherit
      margin         =: 16px
      font-height    =: 16px
      line-height    =: 20.8px
      display        =: block
      padding-bottom =: 2px

    has ".sourceLine" .> do
      white-space =: pre-wrap
      display     =: inline-block
      line-height =: 1.5em
      width       =: (100%)

    has (tag Code) . has (tag Span) $ do
      is ".co" .> do { color =: mono3; font-style =: italic }                         -- Comment
      is ".dt" .> color =: orange2                                                   -- DataType
      is ".kw" .> do { color =: blue_   }                                            -- Keyword
      is ".cf" .> do { color =: purple_ }                                            -- ControlFlow
      is ".op" .> color =: blue_                                                     -- Operator
      -- is ".ot" .> color =: mono1                                                     -- Other
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
      is ".fu" .> color =: blue_                                                     -- Function
      is ".al" .> do { color =: red2; font-weight =: bold }                           -- Alert
      is ".er" .> do { color =: red2; font-weight =: bold }                           -- Error
      is ".wa" .> do { color =: red1; font-weight =: bold; font-style =: italic }      -- Warning
      is ".im" .> color =: purple_                                                   -- Import
      is ".bu" .> color =: purple_                                                   -- BuiltIn
      is ".ex" .> color =: purple_                                                   -- Extension
      is ".do" .> do { color =: mono3; font-style =: italic }                         -- Documentation
      is ".an" .> do { color =: purple_; font-weight =: bold; font-style =: italic }   -- Annotation
      is ".cv" .> do { color =: mono3; font-weight =: bold; font-style =: italic }     -- CommentVar
      is ".in" .> do { color =: mono3; font-weight =: bold; font-style =: italic }     -- Information