{-# language CPP #-}
module Components.CopyToClipboard where

import Pure.Elm
import Pure.Data.SVG (pattern Svg)
import Components.Icons ( copyIcon, checkIcon )
import Styles.Colors ( base )

import Data.Coerce ( coerce )

#ifdef __GHCJS__
foreign import javascript unsafe
  "var range = document.createRange(); range.selectNode($1); window.getSelection().removeAllRanges(); window.getSelection().addRange(range); document.execCommand('copy'); window.getSelection().removeAllRanges();" 
  copy_to_clipboard_js :: Node -> IO ()
#endif

copyToClipboard :: Node -> IO ()
copyToClipboard n =
#ifdef __GHCJS__
  copy_to_clipboard_js n
#else
  pure ()
#endif

data Msg = Copied | Received
copyable :: View -> View
copyable = run (App [] [Received] [] mdl update view) 
  where
    mdl = False
    update Copied _ _ = pure True
    update Received _ _ = pure False
    view v b =
      let 
        handleClick evt = do
          copyToClipboard (coerce $ evtTarget evt)
          command Copied

        (cls,img) 
          | b = (Class "copied",checkIcon)
          | otherwise = (OnClick handleClick,copyIcon)

      in
        Div <| Themed @CopyableT . cls |> 
          [ img
          , v 
          ]

data CopyableT = CopyableT
instance Theme CopyableT where
  theme c = is c do
    position =: relative

    child (tag Svg) do
      fill       =: toTxt base
      position   =: absolute
      right      =: -10px
      top        =: 10px
      height     =: 32px
      opacity    =: 0
      transition =* [opacity,300ms]

    isn't ".copied" do
      hover do 
        child (tag Svg) do 
          opacity =: 1

    is ".copied" do
      child (tag Svg) do
        opacity =: 1

processCopyable :: View -> View
processCopyable (Classes cs (Children vs v))
  | "sourceCode" `elem` cs = copyable v
  | otherwise = SetChildren (fmap processCopyable vs) v
  