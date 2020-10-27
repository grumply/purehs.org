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

import Prelude hiding (break,max,even,or)
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
  theme c =
    is c do
      width  =: (100%)
      margin =* [0,auto]

      mediumScreens <%> do
        width =: 520px

      largeScreens <%> do
        width =: 700px

      within @PlaceholderT do
        filter_ =: blur(8px)


data EditorT deriving Theme
data MarkdownT
instance Theme MarkdownT where
  theme c = 
    is c do
      -- for inheriting
      color =: toTxt black
      width =: (100%)

      has ".hide" do
        display =: none

      has ".footnotes" do
        margin-top =: 1em

      has (subtheme @EditorT) do
        width  =: (100%)

        has (tag A) do
          color           =: toTxt gray
          text-decoration =: none
          border          =: none
          background      =: white

          hover do
            color =: toTxt green
            background      =: white
            border          =: none

          visited do
            color =: toTxt gray
            background      =: white
            border          =: none

            hover do
              color =: toTxt green
              background      =: white
              border          =: none

      has (tag Figure) do
        margin     =* [30px,0]
        text-align =: center

        largeScreens <%> do
          margin =* [30px,(-80)px]
          
        has (tag Img) do
          max-width =: (100%)
          height    =: auto

        has (tag Figcaption) do
          text-align  =: center
          margin-top  =: 8px
          color       =: toTxt black
          font-family =: defaultFont
          font-size   =: 16px

      has (tag P) do
        line-height =: 28px
        font-size   =: 16px
        font-weight =: 400
        color       =: toTxt black
        margin      =: 16px

      has (tag A) do
        background      =: toTxt (faded green)
        border-bottom   =* [1px,solid,toTxt green]
        color           =: toTxt black
        text-decoration =: none

        hover do
          background    =: toTxt green

        visited do
          background    =: toTxt (faded green)

          hover do
            background  =: toTxt green

      has (tag Li) do
        line-height =: 20px
        font-size   =: 18px
        font-weight =: 400
        color       =: toTxt base { brightness = 25 }
        margin      =: 16px

      has (tag H1) do
        font-family =: titleFont
        font-size   =: 48px
        margin      =: 0

      has (tag H2) do
        has (tag A) do
          or is c do
            has (tag H3) do
              has (tag A) do
                important do
                  background =: none
                  border-bottom =: none

                hover do
                  important do
                    color =: toTxt green
                    background =: none
                    border-bottom =: none

                visited do
                  hover do 
                    important do
                      color =: toTxt green
                      background =: none
                      border-bottom =: none

      has (tag H2) do
        font-family =: titleFont
        font-size =: 32px

      has (tag H3) do
        font-family =: titleFont
        color       =: toTxt base { brightness = 45 }
        font-size   =: 24px
        padding-top =: 1em

      has (tag P) do
        has (tag Code) do
          font-family =: defaultMonoFont
          word-break  =: break-word
          padding     =* [3px,4px,2px,4px]
          background  =: toTxt (faded orange)

      has (tag Li) do 
        has (tag Code) do
          font-family =: defaultMonoFont
          word-break  =: break-word
          padding     =* [3px,4px,2px,4px]
          background  =: toTxt (faded orange)

      has (tag Blockquote) do
        border-left      =* [5px,solid,toTxt black]
        padding          =* [16px,0px,16px,8px]
        margin           =* [20px,0px,20px,(-16)px]

        child (tag P) do
          margin-bottom =: 0

        atMedia "(max-width: 48em)" do
          margin-right =: (-16)px

          has ".sourceCode" do 
            has (tag Pre) do
              margin-right =: 0

        atMedia "(min-width: 48em)" do
          margin-left  =: 16px
          margin-right =: 16px

        has (tag H2) do
          font-family =: titleFont
          margin    =: 8px
          font-size =: 20px

        has (tag H3) do
          font-family =: titleFont
          border-top =: none
          margin     =: 8px
          font-size  =: 18px

        has (tag P) do
          font-style =: italic
          font-weight =: 400
          font-family =: serifFont
          font-size   =: 18px
          color =: toTxt gray
          margin-top =: 0

      has ".warn" do
        background-color =: toTxt (faded orange)
        border-left      =* [10px,solid,toTxt red]
        padding          =* [16px,0px,16px,16px]
        margin           =* [20px,0px,20px,(-16)px]

        child (tag P) do
          margin-bottom =: 0

        atMedia "(max-width: 48em)" do
          margin-right =: (-16)px

          has ".sourceCode" do 
            has (tag Pre) do
              margin-right =: 0

        atMedia "(min-width: 48em)" do
          margin-left  =: 16px
          margin-right =: 16px

        has (tag H2) do
          padding-top =: 0
          font-family =: titleFont
          margin    =: 8px
          font-size =: 20px

        has (tag H3) do
          padding-top =: 0
          font-family =: titleFont
          border-top =: none
          margin     =: 8px
          font-size  =: 18px

        has (tag P) do
          margin-top =: 0

      has ".info" do
        background-color =: toTxt (faded blue)
        border-left      =* [10px,solid,toTxt blue]
        padding          =* [16px,0px,16px,16px]
        margin           =* [20px,0px,20px,(-16)px]

        child (tag P) do
          margin-bottom =: 0

        atMedia "(max-width: 48em)" do
          margin-right =: (-16)px

          has ".sourceCode" do 
            has (tag Pre) do
              margin-right =: 0

        atMedia "(min-width: 48em)" do
          margin-left  =: 16px
          margin-right =: 16px

        has (tag H2) do
          padding-top =: 0
          font-family =: titleFont
          margin    =: 8px
          font-size =: 20px

        has (tag H3) do
          padding-top =: 0
          font-family =: titleFont
          border-top =: none
          margin     =: 8px
          font-size  =: 18px

        has (tag P) do
          margin-top =: 0

      has ".more" do
        has (tag A) do
          display       =: block
          text-align    =: right
          margin-right  =: 16px
          margin-top    =: 30px
          color         =: toTxt black
          border-bottom =: none
          background    =: none
          font-size     =: 18px

          hover do
            color =: toTxt green
            background =: none

          visited do
            color =: toTxt black
            background =: none

          visited do 
            hover do
              color =: toTxt green
              background =: none

      has ".prev" do
        or is c do
          has ".next" do
            position =: absolute
            bottom =: 20px

            has (tag A) do
              display       =: block
              margin-top    =: 8px
              color         =: toTxt black
              border-bottom =: none
              background    =: none
              font-size     =: 18px

              hover do
                color =: toTxt green
                background =: none

              visited do
                color =: toTxt black
                background =: none

              visited do 
                hover do
                  color =: toTxt green
                  background =: none

      has ".prev" do
        left =: 8vw
        has (tag A) do
          float         =: left
          margin-left   =: (-16)px

      has ".next" do
        right =: 8vw
        has (tag A) do
          float         =: right
          margin-right  =: (-16)px

      has (tag Pre) do 
        is ".sourceCode" do
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

      has (tag Pre) do 
        is ".sourceCode" do 
          is "::-webkit-scrollbar" do 
            important do
              width  =: 0
              height =: 0

        atMedia "(min-width: 48em)" do
          border-radius =: 10px
          margin-left   =: 16px
          margin-right  =: 16px

      has (tag Code) do 
        is ".sourceCode" do
          font-family    =: inherit
          margin         =: 16px
          font-height    =: 16px
          line-height    =: 20.8px
          display        =: block
          padding-bottom =: 2px

      has ".sourceLine" do
        white-space =: pre-wrap
        display     =: inline-block
        line-height =: 1.5em
        width       =: (100%)

      has (tag Code) do 
        has (tag Span) do

          -- Comment
          is ".co" do 
            color =: mono3
            font-style =: italic

          -- DataType
          is ".dt" do 
            color =: orange2

          -- Keyword
          is ".kw" do 
            color =: blue_

          -- ControlFlow
          is ".cf" do 
            color =: purple_ 

          -- Operator
          is ".op" do 
            color =: blue_                                                     

          -- Other
          -- is ".ot" .> color =: mono1 

          -- SpecialChar
          is ".sc" do 
            color =: blue_ 

          -- SpecialString
          is ".ss" do 
            color =: blue_ 

          -- VerbatimString
          is ".vs" do 
            color =: blue_ 

          -- Constant
          is ".cn" do 
            color =: orange1 

          -- DecVal
          is ".dv" do 
            color =: orange1 

          -- BaseN
          is ".bn" do 
            color =: orange1 

          -- Float
          is ".fl" do 
            color =: orange1 

          -- Char
          is ".ch" do 
            color =: orange1 

          -- String
          is ".st" do 
            color =: green_ 

          -- Variable
          is ".va" do 
            color =: red1 

          -- Function
          is ".fu" do 
            color =: blue_ 

          -- Alert
          is ".al" do 
            color =: red2
            font-weight =: bold 

          -- Error
          is ".er" do 
            color =: red2
            font-weight =: bold 

          -- Warning
          is ".wa" do 
            color =: red1
            font-weight =: bold
            font-style =: italic 

          -- Import
          is ".im" do 
            color =: purple_ 

          -- BuiltIn
          is ".bu" do 
            color =: purple_ 

          -- Extension
          is ".ex" do 
            color =: purple_ 

          -- Documentation
          is ".do" do 
            color =: mono3
            font-style =: italic 

          -- Annotation
          is ".an" do 
            color =: purple_
            font-weight =: bold
            font-style =: italic 

          -- CommentVar
          is ".cv" do 
            color =: mono3
            font-weight =: bold
            font-style =: italic 

          -- Information
          is ".in" do 
            color =: mono3
            font-weight =: bold
            font-style =: italic 