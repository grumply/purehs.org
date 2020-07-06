{-# language DeriveAnyClass #-}
module Components.Editor (editor) where

import Shared
import Styles.Fonts
import Styles.Colors

import qualified Pure.Elm as Elm
import Pure.Elm hiding (state,mode,code,Default,green,gray)
import Pure.WebSocket (WebSocket,clientWS,remote)

import Data.Foldable
import System.IO.Unsafe

{-# NOINLINE tryWS #-}
tryWS :: WebSocket
tryWS = unsafePerformIO $ clientWS "204.48.20.19" 8080

newtype Editor = Editor JSV
  deriving Elm.Default
    via JSV

foreign import javascript unsafe
  "var f = function(cm) { cm['replaceSelection']('  ','end'); };var o = {}; o['theme'] = 'one-dark'; o['viewportMargin'] = Infinity; o['mode'] = 'text/x-haskell'; o['lineNumbers'] = true; o['tabSize'] = 2; o['extraKeys'] = {};  o['extraKeys']['Tab'] = f; o['fixedGutter'] = false; $r = CodeMirror['fromTextArea']($1,o)" 
    start_editor_js 
      :: Node -> IO Editor

foreign import javascript unsafe
  "$r = CodeMirror['fromTextArea']($1)" 
    start_viewer_js 
      :: Node -> IO Editor

foreign import javascript unsafe
  "$1['getValue']()"
    get_value_js
      :: Editor -> IO Txt

foreign import javascript unsafe
  "$1['setValue']($2);"
    set_value_js
      :: Editor -> Txt -> IO ()

foreign import javascript unsafe
  "$1['setOption']('keyMap','vim'); $1['setOption']('vimMode',true);"
    set_vim_js
      :: Editor -> IO ()

foreign import javascript unsafe
  "$1['clearHistory']()"
    clear_history_js
      :: Editor -> IO ()

foreign import javascript unsafe
  "$1['setOption']('keyMap','sublime');"
    set_sublime_js
      :: Editor -> IO ()

foreign import javascript unsafe
  "$1['setOption']('keyMap','default');"
    set_default_js
      :: Editor -> IO ()

foreign import javascript unsafe
  "$1['refresh']();" 
    refresh_js
      :: Editor -> IO ()

data Mode = Editing | Compiling

data State = Waiting | Failure Txt | Success String 

data Keymap = Vim | Sublime | Default

data Model = Model 
  { mode      :: Mode
  , state     :: State
  , keymap    :: Keymap
  , code      :: Editor
  , results   :: Editor
  }

data Msg 
  = CreateCodeView Node 
  | CreateResultsView Node 
  | Compile | Edit | SetKeymap Keymap
  | Failed Txt | Succeeded String 
  | Receive | Refresh

editor :: View -> View
editor = run (App [] [Receive] [] (Model Editing Waiting Default def def) update view)
  where
    update :: Elm Msg => Msg -> View -> Model -> IO Model
    update Receive _ mdl = do
      pure mdl 
        { mode = Editing
        , state = Waiting 
        }

    update (CreateCodeView n) _ mdl = do
      e <- start_editor_js n
      pure mdl
        { code = e 
        }

    update (CreateResultsView n) _ mdl = do
      e <- start_viewer_js n
      pure mdl 
        { results = e 
        }

    update Refresh _ mdl = do
      refresh_js (code mdl)
      refresh_js (results mdl)
      pure mdl

    update Compile _ mdl = do
      v <- get_value_js (code mdl)
      set_value_js (results mdl) "Compiling..."
      remote compileAPI tryWS compile (v,False) 
        (command . either Failed Succeeded)
      command Refresh
      pure mdl 
        { mode = Compiling
        , state = Waiting 
        }

    update Edit _ mdl = do
      command Refresh
      pure mdl 
        { mode = Editing }

    update (Failed failed) _ mdl = do
      set_value_js (results mdl) failed
      pure mdl 
        { state = Failure failed 
        }

    update (Succeeded success) _ mdl = do
      pure mdl 
        { state = Success success }

    update (SetKeymap km) _ mdl = do
      case km of
        Vim     -> set_vim_js     (code mdl)
        Sublime -> set_sublime_js (code mdl)
        Default -> set_default_js (code mdl)
      pure mdl { keymap = km }

    view v mdl = 
      let modeT =
            case mode mdl of
              Compiling -> Themed @CompilingT 
              Editing   -> Themed @EditingT

          stateT =
            case state mdl of
              Waiting   -> Themed @WaitingT
              Success _ -> Themed @SuccessT
              Failure _ -> Themed @FailureT

          src r = "http://try.purehs.org/static/builds/" 
               <> toTxt r 
               <> "/Main.jsexe/index.html"

      in Div <| Themed @EditorT . modeT . stateT |>
          [ Div <| Themed @CommandsT |>
            [ Span <||>
              [ Span <| Themed @CommandT . Themed @EditButtonT    . OnClick (\_ -> command Edit)    |> [ "Edit" ] 
              , Span <| Themed @CommandT . Themed @CompileButtonT . OnClick (\_ -> command Compile) |> [ "Compile" ]
              , case keymap mdl of
                  Vim     -> Span <| Themed @CommandT . Themed @KeymapButtonT . OnClick (\_ -> command (SetKeymap Sublime)) |> [ "Sublime" ]
                  Sublime -> Span <| Themed @CommandT . Themed @KeymapButtonT . OnClick (\_ -> command (SetKeymap Default)) |> [ "Default" ]
                  Default -> Span <| Themed @CommandT . Themed @KeymapButtonT . OnClick (\_ -> command (SetKeymap Vim))     |> [ "Vim" ]
              ]
            , A <| Themed @EditorBrandingT . Href "http://try.purehs.org"  . Attribute "target" "_blank" |> [ "try.purehs.org" ]
            ]
          , Div <| Themed @CodeT |> 
            [ v <| WithHost (command . CreateCodeView)
            ]
          , Div <| Themed @ResultsT |>
            [ Textarea <| WithHost (command . CreateResultsView)
            ]
          , case state mdl of
              Success success -> Iframe <| Themed @FrameT . Src (src success)
              _ -> Null
          ]

-- States
data CompilingT deriving Theme
data EditingT   deriving Theme

data WaitingT   deriving Theme
data SuccessT   deriving Theme
data FailureT   deriving Theme

-- Components
data CommandsT
instance Theme CommandsT where
  theme c = void $ is c .> do
    padding =: 8px
    display =: flex
    justify-content =: space-between
    background-color =: white
    width =: (100%)
    height =: 36px

data CommandT
instance Theme CommandT where
  theme c = void $ 
    is c $ do
      apply $ do
        cursor       =: pointer
        font-size    =: 18px
        margin-right =: 12px
        font-family  =: defaultFont
        color        =: toTxt gray
      
      is hover .> do
        color =: toTxt green

data EditorBrandingT
instance Theme EditorBrandingT where
  theme c = void $ is c $ do
    apply $ do
      display         =: inline-block
      cursor          =: pointer
      font-size       =: 18px
      color           =: toTxt gray
      text-decoration =: none

    is hover .> do
      color =: toTxt green

    is hover . is visited .> do
      color =: toTxt green

    is visited .> do
      color =: toTxt gray
 
data EditButtonT
instance Theme EditButtonT where
  theme c = void $ do
    is c .> do
      display =: none
      height  =: 20px
      
    is (subtheme @CompilingT) $ 
      has c .> do
        display =: inline-block

data CompileButtonT
instance Theme CompileButtonT where
  theme c = void $ do
    is c .> do
      display =: none
      height  =: 20px

    has (subtheme @EditingT) $
      has c .> do
        display =: inline-block

data KeymapButtonT 
instance Theme KeymapButtonT where
  theme c = void $ do
    is c .> do
      display =: none
      height  =: 20px

    has (subtheme @EditingT) $
      has c .> do
        display =: inline-block

data CodeT
instance Theme CodeT where
  theme c = void $ do
    is c .> do
      display =: none
      width =: (100%)
      height =: calc((100%) - 36px)

    has (subtheme @EditingT) $
      has c .> do
        display =: block

data ResultsT
instance Theme ResultsT where
  theme c = void $ do
    is c .> do
      display =: none
      width =: (100%)
      height =: calc((100%) - 36px)

    is (subtheme @CompilingT) $ do
      has c .> do
        display =: block
      
      is (subtheme @SuccessT) $
        has c .> do
          display =: none

data FrameT
instance Theme FrameT where
  theme c = void $  do
    is c $ do
      apply $ do
        display =: none
        background-color =: white
        outline =: none
        border =: none
        width =: (100%)
        height =: calc((100%) - 36px)
        padding-bottom =: 8px

    is (subtheme @CompilingT) $
      is (subtheme @SuccessT) $
        has c .> do
          display =: block

data EditorT
instance Theme EditorT where
  theme c = void $ do
    is ".CodeMirror" .> do
      height =: (100%)
    
    is c .> do
      border =* [1px,solid,toTxt gray]
      box-shadow =* [0px,4px,10px,toTxt gray]
      height =: (100%)
