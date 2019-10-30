# pure-events

`pure-events` implements convenience patterns and methods for working with event listeners.


## Pure.Data.Events

`Pure.Data.Events` takes a unified approach to event listeners with an `On_` pattern for any particular event as well as an `On_With` pattern when configuration is required for that event.

The patterns are defined in two tiers.

The first tier are the core patterns, `On`, `OnWith`, `OnDoc`, `OnDocWith`, `OnWin`, and `OnWinWith`. Since listeners are connected to views, they may create host element, window, and document listeners that will be cleaned up when the view is removed, thus the need for all three sets of variants. That is, window- and document-targeted event listeners don't have to be side-loaded and separately managed - they can be created and removed automatically when the view is mounted or unmounted, just like host-targeted listeners.

The second tier are the targeted patterns, e.g. `OnClick` and `OnClickWith`, that are defined in terms of `On` and `OnWith`.

> NOTE: These patterns will not correctly extract listeners from views. The following.
>
> ```haskell
> getClickListener :: View -> Maybe (Evt -> IO ())
> getClickListener (OnClick f Button) = Just f
> getClickListener _ = Nothing
> ```
>
> Will not currently work. These patterns are meant as a convenience for constructing views. But one-way constructor patterns are not supported in GHC, so the destructuring part of each of these bi-directional patterns is a no-op shim that always matches. The above `getClickListener` will always return `Just (\_ -> pure ())` if given a `Button :: View`.

### pattern On

`On` is a generic pattern for constructing listeners of a given event by name.

```haskell
pattern On :: HasFeatures a => Txt -> (Evt -> IO ()) -> a -> a
```

```haskell
button = Button <| On "click" (\_ -> print "Clicked!" |> [ "Click Me!" ]
```

### pattern OnWith

`OnWith` extends `On` with listener `Options`.

```haskell
pattern OnWith :: HasFeatures a => Options -> Txt -> (Evt -> IO ()) -> a -> a
```

Note the listener `Options`.

```haskell
data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  }
```

### pattern OnDoc

`OnDoc` is a generic pattern for constructing document listeners of a given event by name.

```haskell
pattern OnDoc :: HasFeatures a => Txt -> (Evt -> IO ()) -> a -> a
```

### pattern OnDocWith

`OnDocWith` extends `OnDoc` with listener `Options`.

```haskell
pattern OnDocWith :: HasFeatures a => Options -> Txt -> (Evt -> IO ()) -> a -> a
```

Note the listener `Options`.

```haskell
data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  }
```

### pattern OnWin

`OnWin` is a generic pattern for constructing window listeners of a given event by name.

```haskell
pattern OnWin :: HasFeatures a => Txt -> (Evt -> IO ()) -> a -> a
```

```haskell
content = Div <| OnWin "resize" (\_ -> print "Window size changed.")
```

### pattern OnWinWith

`OnWinWith` extends `OnWin` with listener `Options`.

```haskell
pattern OnWinWith :: HasFeatures a => Options -> Txt -> (Evt -> IO ()) -> a -> a
```

Note the listener `Options`.

```haskell
data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  }
```

### pattern OnResize

`OnResize` is a window listener for the [resize](https://developer.mozilla.org/en-US/docs/Web/API/Window/resize_event) event.

```haskell
pattern OnResize :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnResize f a = OnWin "resize" f a
```

### pattern OnResizeWith

`OnResizeWith` is a window listener for the [resize](https://developer.mozilla.org/en-US/docs/Web/API/Window/resize_event) event with listener `Options`.

```haskell
pattern OnResizeWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnResizeWith opts f a = OnWinWith opts "resize" f a
```

### pattern OnScroll

`OnScroll` is a window listener for the [scroll](https://developer.mozilla.org/en-US/docs/Web/API/Document/scroll_event) event.

```haskell
pattern OnScroll :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnScroll f a = OnWin "scroll" f a
```

### pattern OnScrollWith

`OnScrollWith` is a window listener for the [scroll](https://developer.mozilla.org/en-US/docs/Web/API/Document/scroll_event) event with listener `Options`.

```haskell
pattern OnScrollWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnScrollWith opts f a = OnWinWith opts "scroll" f a
```

### pattern OnClose

`OnClose` is a window listener for the [close](https://developer.mozilla.org/en-US/docs/Archive/Mozilla/XUL/Events/close_event) event.

```haskell
pattern OnClose :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnClose f a = OnWin "close" f a
```

### pattern OnCloseWith

`OnCloseWith` is a window listener for the [close](https://developer.mozilla.org/en-US/docs/Archive/Mozilla/XUL/Events/close_event) event with listener `Options`.

```haskell
pattern OnCloseWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnCloseWith opts f a = OnWinWith opts "close" f a
```

### pattern OnBeforeUnload

`OnBeforeUnload` is a window listener for the [beforeunload](https://developer.mozilla.org/en-US/docs/Web/API/Window/beforeunload_event) event.

```haskell
pattern OnBeforeUnload :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnBeforeUnload f a = OnWin "beforeunload" f a
```

### pattern OnBeforeUnloadWith

`OnBeforeUnloadWith` is a window listener for the [beforeunload](https://developer.mozilla.org/en-US/docs/Web/API/Window/beforeunload_event) event with listener `Options`.

```haskell
pattern OnBeforeUnloadWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnBeforeUnloadWith opts f a = OnWinWith opts "beforeunload" f a
```

### pattern OnClick

`OnClick` is an element listener for the [click](https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event) event.

```haskell
pattern OnClick :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnClick f a = On "click" f a
```

### pattern OnClickWith

`OnClickWith` is an element listener for the [click](https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event) event with listener `Options`.

```haskell
pattern OnClickWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnClickWith opts f a = OnWith opts "click" f a
```

### pattern OnDoubleClick

`OnDoubleClick` is an element listener for the [dblclick](https://developer.mozilla.org/en-US/docs/Web/API/Element/dblclick_event) event.

```haskell
pattern OnDoubleClick :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnDoubleClick f a = On "dblclick" f a
```

### pattern OnDoubleClickWith

`OnDoubleClickWith` is an element listener for the [dblclick](https://developer.mozilla.org/en-US/docs/Web/API/Element/dblclick_event) event with listener `Options`.

```haskell
pattern OnDoubleClickWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnDoubleClickWith opts f a = OnWith opts "dblclick" f a
```

### pattern OnMouseDown

`OnMouseDown` is an element listener for the [mousedown](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousedown_event) event.

```haskell
pattern OnMouseDown :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseDown f a = On "mousedown" f a
```

### pattern OnMouseDownWith

`OnMouseDownWith` is an element listener for the [mousedown](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousedown_event) event with listener `Options`.

```haskell
pattern OnMouseDownWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseDownWith opts f a = OnWith opts "mousedown" f a
```

### pattern OnMouseUp

`OnMouseUp` is an element listener for the [mouseup](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseup_event) event.

```haskell
pattern OnMouseUp :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseUp f a = On "mouseup" f a
```

### pattern OnMouseUpWith

`OnMouseUpWith` is an element listener for the [mouseup](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseup_event) event with listener `Options`.

```haskell
pattern OnMouseUpWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseUpWith opts f a = OnWith opts "mouseup" f a
```

### pattern OnTouchStart

`OnTouchStart` is an element listener for the [touchstart](https://developer.mozilla.org/en-US/docs/Web/API/Document/touchstart_event) event.

```haskell
pattern OnTouchStart :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchStart f a = On "touchstart" f a
```

### pattern OnTouchStartWith

`OnTouchStartWith` is an element listener for the [touchstart](https://developer.mozilla.org/en-US/docs/Web/API/Document/touchstart_event) event with listener `Options`.

```haskell
pattern OnTouchStartWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchStartWith opts f a = OnWith opts "touchstart" f a
```

### pattern OnTouchEnd

`OnTouchEnd` is an element listener for the [touchend](https://developer.mozilla.org/en-US/docs/Web/API/Document/touchend_event) event.

```haskell
pattern OnTouchEnd :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchEnd f a = On "touchend" f a
```

### pattern OnTouchEndWith

`OnTouchEndWith` is an element listener for the [touchend](https://developer.mozilla.org/en-US/docs/Web/API/Document/touchend_event) event with listener `Options`.

```haskell
pattern OnTouchEndWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchEndWith opts f a = OnWith opts "touchend" f a
```

### pattern OnMouseEnter

`OnMouseEnter` is an element listener for the [mouseenter](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseenter_event) event.

```haskell
pattern OnMouseEnter :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseEnter f a = On "mouseenter" f a
```

### pattern OnMouseEnterWith

`OnMouseEnterWith` is an element listener for the [mouseenter](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseenter_event) event with listener `Options`.

```haskell
pattern OnMouseEnterWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseEnterWith opts f a = OnWith opts "mouseenter" f a
```

### pattern OnMouseLeave

`OnMouseLeave` is an element listener for the [mouseleave](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseleave_event) event.

```haskell
pattern OnMouseLeave :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseLeave f a = On "mouseleave" f a
```

### pattern OnMouseLeaveWith

`OnMouseLeaveWith` is an element listener for the [mouseleave](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseleave_event) event with listener `Options`.

```haskell
pattern OnMouseLeaveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseLeaveWith opts f a = OnWith opts "mouseleave" f a
```

### pattern OnMouseOver

`OnMouseOver` is an element listener for the [mouseover](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseover_event) event.

```haskell
pattern OnMouseOver :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseOver f a = On "mouseover" f a
```

### pattern OnMouseOverWith

`OnMouseOverWith` is an element listener for the [mouseover](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseover_event) event with listener `Options`.

```haskell
pattern OnMouseOverWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseOverWith opts f a = OnWith opts "mouseover" f a
```

### pattern OnMouseOut

`OnMouseOut` is an element listener for the [mouseout](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseout_event) event.

```haskell
pattern OnMouseOut :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseOut f a = On "mouseout" f a
```

### pattern OnMouseOutWith

`OnMouseOutWith` is an element listener for the [mouseout](https://developer.mozilla.org/en-US/docs/Web/API/Element/mouseout_event) event with listener `Options`.

```haskell
pattern OnMouseOutWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseOutWith opts f a = OnWith opts "mouseout" f a
```

### pattern OnMouseMove

`OnMouseMove` is an element listener for the [mousemove](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event) event.

```haskell
pattern OnMouseMove :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnMouseMove f a = On "mousemove" f a
```

### pattern OnMouseMoveWith

`OnMouseMoveWith` is an element listener for the [mousemove](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event) event with listener `Options`.

```haskell
pattern OnMouseMoveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnMouseMoveWith opts f a = OnWith opts "mousemove" f a
```

### pattern OnTouchMove

`OnTouchMove` is an element listener for the [touchmove](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchmove_event) event.

```haskell
pattern OnTouchMove :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchMove f a = On "touchmove" f a
```

### pattern OnTouchMoveWith

`OnTouchMoveWith` is an element listener for the [touchmove](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchmove_event) event with listener `Options`.

```haskell
pattern OnTouchMoveWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchMoveWith opts f a = OnWith opts "touchmove" f a
```

### pattern OnTouchCancel

`OnTouchCancel` is an element listener for the [touchcancel](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchcancel_event) event.

```haskell
pattern OnTouchCancel :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnTouchCancel f a = On "touchcancel" f a
```

### pattern OnTouchCancelWith

`OnTouchCancelWith` is an element listener for the [touchcancel](https://developer.mozilla.org/en-US/docs/Web/API/Element/touchcancel_event) event with listener `Options`.

```haskell
pattern OnTouchCancelWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnTouchCancelWith opts f a = OnWith opts "touchcancel" f a
```

### pattern OnBlur

`OnBlur` is an element listener for the [blur](https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event) event.

```haskell
pattern OnBlur :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnBlur f a = On "blur" f a
```

### pattern OnBlurWith

`OnBlurWith` is an element listener for the [blur](https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event) event with listener `Options`.

```haskell
pattern OnBlurWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnBlurWith opts f a = OnWith opts "blur" f a
```

### pattern OnFocus

`OnFocus` is an element listener for the [focus](https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event) event.

```haskell
pattern OnFocus :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnFocus f a = On "focus" f a
```

### pattern OnFocusWith

`OnFocusWith` is an element listener for the [focus](https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event) event with listener `Options`.

```haskell
pattern OnFocusWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnFocusWith opts f a = OnWith opts "focus" f a
```

### pattern OnInput

`OnInput` is an element listener for the [input](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event) event. This event is expected to be used with the `withInput` helper.

```haskell
pattern OnInput :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnInput f a = On "input" f a
```

### pattern OnInputWith

`OnInputWith` is an element listener for the [input](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event) event with listener `Options`. This event is expected to be used with the `withInput` helper.

```haskell
pattern OnInputWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnInputWith opts f a = OnWith opts "input" f a
```

### pattern OnChange

`OnChange` is an element listener for the [change](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event) event. This event is expected to be combined with the `withInput` or the `withChecked` helper.

```haskell
pattern OnChange :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnChange f a = On "change" f a
```

### pattern OnChangeWith

`OnChangeWith` is an element listener for the [change](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event) event with listener `Options`. This event is expected to be combined with the `withInput` or the `withChecked` helper.

```haskell
pattern OnChangeWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnChangeWith opts f a = OnWith opts "change" f a
```

### pattern OnCheck

`OnCheck` is an element listener for the [change](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event) event. This is a synonym for the `OnChange` pattern. This is expected to be paired with the `withChecked` helper.

```haskell
pattern OnCheck :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnCheck f a = On "change" f a
```

### pattern OnCheckWith

`OnCheckWith` is an element listener for the [change](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event) event with listener `Options`. This event is expected to be paired with the `withChecked` helper.

```haskell
pattern OnCheckWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnCheckWith opts f a = OnWith opts "change" f a
```

### pattern OnSubmit

`OnSubmit` is an element listener for the [submit](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event) event.

```haskell
pattern OnSubmit :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnSubmit f a = On "submit" f a
```

### pattern OnSubmitWith

`OnSubmitWith` is an element listener for the [submit](https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event) event with listener `Options`.

```haskell
pattern OnSubmitWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnSubmitWith opts f a = OnWith opts "submit" f a
```

### pattern OnKeyUp

`OnKeyUp` is an element listener for the [keyup](https://developer.mozilla.org/en-US/docs/Web/API/Document/keyup_event) event.

```haskell
pattern OnKeyUp :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyUp f a = On "keyup" f a
```

### pattern OnKeyUpWith

`OnKeyUpWith` is an element listener for [keyup](https://developer.mozilla.org/en-US/docs/Web/API/Document/keyup_event) event with listener `Options`.

```haskell
pattern OnKeyUpWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyUpWith opts f a = OnWith opts "keyup" f a
```

### pattern OnKeyDown

`OnKeyDown` is an element listener for the [keydown](https://developer.mozilla.org/en-US/docs/Web/API/Document/keydown_event) event.

```haskell
pattern OnKeyDown :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyDown f a = On "keydown" f a
```

### pattern OnKeyDownWith

`OnKeyDownWith` is an element listener for [keydown](https://developer.mozilla.org/en-US/docs/Web/API/Document/keydown_event) event with listener `Options`.

```haskell
pattern OnKeyDownWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyDownWith opts f a = OnWith opts "keydown" f a
```

### pattern OnKeyPress

`OnKeyPress` is an element listener for the [keypress](https://developer.mozilla.org/en-US/docs/Web/API/Document/keypress_event) event.

```haskell
pattern OnKeyPress :: HasFeatures a => (Evt -> IO ()) -> a -> a
pattern OnKeyPress f a = On "keypress" f a
```

### pattern OnKeyPressWith

`OnKeyPressWith` is an element listener for the [keypress](https://developer.mozilla.org/en-US/docs/Web/API/Document/keypress_event) event with listener `Options`.

```haskell
pattern OnKeyPressWith :: HasFeatures a => Options -> (Evt -> IO ()) -> a -> a
pattern OnKeyPressWith opts f a = OnWith opts "keypress" f a
```

### passive

`passive` is a pre-configured `Options` for use with `On_With` listeners.

```haskell
passive :: Options
passive = Options False False True
```

### intercept

`intercept` is a pre-configured `Options` for use with `On_With` listeners.

```haskell
intercept :: Options
intercept = Options True True False
```

### nodefault

`nodefault` is a pre-configured `Options` for use with `On_With` listeners.

```haskell
nodefault :: Options
nodefault = Options True False False
```

### noprop

`noprop` is a pre-configured `Options` for use with `On_With` listeners.

```haskell
noprop :: Options
noprop = Options False True False
```

### withInput

`withInput` is a utility for extracting and handling a `Coercible Txt a` from a `event.value.target` value inside an input handler.

```haskell
withInput :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withInput f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj
```

```haskell
input :: Coercible Txt a => (a -> IO ()) -> View
input f = Input <| OnInput (withInput f)
```

### withValue

`withValue` is a synonym for `withInput`, a utility for extracting and handling a `Coercible Txt a` from a `event.value.target` value inside an input handler.

```haskell
withValue :: forall a. Coercible Txt a => (a -> IO ()) -> (Evt -> IO ())
withValue f = traverse_ (f . (coerce :: Txt -> a)) . join . fmap (.# "value") . (.# "target") . evtObj
```

```haskell
input :: Coercible Txt a => (a -> IO ()) -> View
input f = Input <| OnInput (withValue f)
```

### withChecked

`withChecked` is a utility for extracting a boolean `event.target.checked` value from a checkbox or radio button within a lsitener.

```haskell
withChecked :: (Bool -> IO ()) -> (Evt -> IO ())
withChecked f = traverse_ f . join . fmap (.# "checked") . (.# "target") . evtObj
```

### keyCode

`keyCode` is a utility for extracting a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of key event.

> NOTE: This method is used in the `Key_` patterns exported from this module and is likely to not be needed directly.

```haskell
keyCode :: Evt -> Maybe Int
keyCode = (.# "keyCode") . evtObj
```

```haskell
ui = Div <| OnKeyPress key |> [ ]
  where
    key (keyCode -> Just 48) = handle0
    key _ = pure ()

    handle0 = _
```

### clientY

`clientY` is a utility for extracting the [clientY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/clientY) from a mouse event that gives the y-position of a the event relative to the browser chrome from the top-left.

> NOTE: This utility can be more conveniently accessed in the `pattern ClientY`.

```haskell
clientY :: Evt -> Maybe Int
clientY = (.# "clientY") . evtObj
```

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (clientY -> Just y) = _
    handleMove _ = pure ()
```

### clientX

`clientX` is a utility for extracting the [clientX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/clientX) from a mouse event that gives the x-position of a the event relative to the browser chrome from the top-left.

> NOTE: This utility can be more conveniently accessed in the `pattern ClientX`.

```haskell
clientX :: Evt -> Maybe Int
clientX = (.# "clientX") . evtObj
```

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (clientX -> Just x) = _
    handleMove _ = pure ()
```

### screenY

`screenY` is a utility for extracting the [screenY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/screenY) from a mouse event that gives the y-position of a the event relative to the user's screen from the top-left.

> NOTE: This utility can be more conveniently accessed in the `pattern ScreenY`.

```haskell
screenY :: Evt -> Maybe Int
screenY = (.# "screenY") . evtObj
```

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (screenY -> Just y) = _
    handleMove _ = pure ()
```

### screenX

`screenX` is a utility for extracting the [screenX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/screenX) from a mouse event that gives the x-position of a the event relative to the user's screen from the top-left.

> NOTE: This utility can be more conveniently accessed in the `pattern ScreenX`.

```haskell
screenY :: Evt -> Maybe Int
screenY = (.# "screenY") . evtObj
```

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (screenY -> Just y) = _
    handleMove _ = pure ()
```

### movementY

`movementY` is a utility for extracting the [movementY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementY) from a mouse event that gives the change in y-position of a [mousemove](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event) event relative to the previous mousemove event.

> NOTE: This utility can be more conveniently accessed in the `pattern MovementY`.

```haskell
movementY :: Evt -> Maybe Int
movementY = (.# "movementY") . evtObj
```

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (movementY -> Just y) = _
    handleMove _ = pure ()
```

### movementX

`movementX` is a utility for extracting the [movementX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX) from a mouse event that gives the change in x-position of a [mousemove](https://developer.mozilla.org/en-US/docs/Web/API/Element/mousemove_event) event relative to the previous mousemove event.

> NOTE: This utility can be more conveniently accessed in the `pattern MovementX`.

```haskell
movementX :: Evt -> Maybe Int
movementX = (.# "movementX") . evtObj
```

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (movementX -> Just x) = _
    handleMove _ = pure ()
```

### pageY

`pageY` is a utility for extracting the [pageY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageY) of mouse event that gives a y-position relative to the top left of the rendered page content.

> NOTE: This utility can be more conveniently accessed in the `pattern PageY`.

```haskell
pageY :: Evt -> Maybe Int
pageY = (.# "pageY") . evtObj
```

```haskell
someView = Div <| OnMouseMove handleMove
  where
    handleMove (pageY -> Just y) = _
    handleMove _ = pure ()
```

### pageX

`pageX` is a utility for extracting the [pageX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageX) of mouse event that gives a x-position relative to the top left of the rendered page content.

> NOTE: This utility can be more conveniently accessed in the `pattern PageX`.

```haskell
pageX :: Evt -> Maybe Int
pageX = (.# "pageX") . evtObj
```

```haskell
someView = Div <| OnMouseMove handleMove
  where
    handleMove (pageX -> Just x) = _
    handleMove _ = pure ()
```

### shift

`shift` is a utility for key events to check for a [shiftKey](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/shiftKey) modifier.

```haskell
shift :: Evt -> Maybe Bool
shift = (.# "shiftKey") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern ShiftKey`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle e@(shiftKey -> Just True) = handleShifted e
    handle _ = pure ()
```

### alt

`alt` is a utility for key events to check for an [altKey](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/altKey) modifier.

```haskell
alt :: Evt -> Maybe Bool
alt = (.# "altKey") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern AltKey`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle e@(altKey -> Just True) = handleAlted e
    handle _ = pure ()
```

### ctrl

`ctrl` is a utility for key events to check for the [ctrlKey](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/ctrlKey) modifier.

```haskell
ctrl :: Evt -> Maybe Bool
ctrl = (.# "ctrlKey") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern CtrlKey`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle e@(ctrlKey -> Just True) = handleCtrled e
    handle _ = pure ()
```

### meta

`meta` is a utility for key events to check for the [metaKey](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/metaKey) modifier.

```haskell
meta :: Evt -> Maybe Bool
meta = (.# "metaKey") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern MetaKey`

```haskell
input = Input <| OnKeyDown handle
  where
    handle e@(metaKey -> Just True) = handleMetaed e
    handle _ = pure ()
```

### button

`button` is a utility for mouse events to inspect the [mouse button](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button) used to perform a mouse event.

```haskell
button :: Evt -> Maybe Int
button = (.# "button") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern LeftButton`, `pattern MiddleButton`, `pattern RightButton`, `pattern BackButton`, and `pattern ForwardButton`.

```haskell
btn = Button <| OnClick handle |> [ "Button" ]
  where
    handle btn
      | Just b <- button btn =
        case b of
          0 -> _ -- left button
          1 -> _ -- middle button
          2 -> _ -- right button
          3 -> _ -- back button
          4 -> _ -- forward button
    handle _ =
      pure ()
```

### target

`target` is a utility for extracting the [target](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/target) from a mouse event.

```haskell
target :: Evt -> Maybe JSV
target = (.# "target") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern Target`.

```haskell
btn = Button <| OnClick handle |> [ "Button" ]
  where
    handle (target -> Just rt) = handleTarget rt
    handle _ = pure ()
```

### relatedTarget

`relatedTarget` is a utility for extracting the [relatedTarget](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/relatedTarget) from a mouse event.

```haskell
relatedTarget :: Evt -> Maybe JSV
relatedTarget = (.# "relatedTarget") . evtObj
```

> NOTE: This utility can be more conveniently accessed in the `pattern RelatedTarget`.

```haskell
btn = Button <| OnClick handle |> [ "Button" ]
  where
    handle (relatedTarget -> Just rt) = handleRelatedTarget rt
    handle _ = pure ()
```

### pattern LeftButton

`LeftButton` is a pattern to match on a left button click for a [mouse](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) event.

```haskell
div = Div <| OnClick handle
  where
    handle (LeftButton (ClientX x (ClientY y _))) = _
    handle _ = pure ()
```

### pattern MiddleButton

`MiddleButton` is a pattern to match on a middle button click for a [mouse](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) event.

```haskell
div = Div <| OnClick handle
  where
    handle (MiddleButton (ClientX x (ClientY y _))) = _
    handle _ = pure ()
```

### pattern RightButton

`RightButton` is a pattern to match on a right button click for a [mouse](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) event.

```haskell
div = Div <| OnClick handle
  where
    handle (RightButton (ClientX x (ClientY y _))) = _
    handle _ = pure ()
```

### pattern BackButton

`BackButton` is a pattern to match on a back button click for a [mouse](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) event.

```haskell
div = Div <| OnClick handle
  where
    handle (BackButton (ClientX x (ClientY y _))) = _
    handle _ = pure ()
```

### pattern ForwardButton

`ForwardButton` is a pattern to match on a forward button click for a [mouse](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) event.

```haskell
div = Div <| OnClick handle
  where
    handle (ForwardButton (ClientX x (ClientY y _))) = _
    handle _ = pure ()
```

### pattern Target

`Target` is a pattern to view the [target](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/target) of a mouse event.

```haskell
div = Div <| OnMouseEnter handle
  where
    handle (Target t _) = withTarget t
    handle _ = pure ()
```

### pattern RelatedTarget

`RelatedTarget` is a pattern to view the [target related](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/relatedTarget) to a mouse event.

```haskell
div = Div <| OnMouseEnter handle
  where
    handle (RelatedTarget old _) = withOldTarget old
    handle _ = pure ()
```

### pattern ShiftModifier

`ShiftModifier` is a pattern to match on a [shift-modified mouse event](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/shiftKey).

```haskell
div = Button <| OnClick handle
  where
    handle (ShiftModifier e) = _
    handle _ = pure ()
```

### pattern AltModifier

`AltModifier` is a pattern to match on a [alt-modified mouse event](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/altKey).

```haskell
div = Button <| OnClick handle
  where
    handle (AltModifier _) = _
    handle _ = pure ()
```

### pattern CtrlModifier

`CtrlModifier` is a pattern to match on a [ctrl-modified mouse event](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/ctrlKey).

```haskell
div = Button <| OnClick handle
  where
    handle (CtrlModifier _) = _
    handle _ = pure ()
```

### pattern MetaModifier

`MetaModifier` is a pattern to match on a [meta-modified mouse event](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/metaKey).

```haskell
div = Button <| OnClick handle
  where
    handle (MetaModifier e) = _
    handle _ = pure ()
```

### pattern ClientY

`ClientY` is a pattern to match on a [clientY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/clientY) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (ClientY y e) = _
    handleMove _ = pure ()
```

### pattern ClientX

`ClientX` is a pattern to match on a [clientX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/clientX) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (ClientX x e) = _
    handleMove _ = pure ()
```

### pattern ScreenY

`ScreenY` is a pattern to match on a [screenY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/screenY) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (ScreenY y e) = _
    handleMove _ = pure ()
```

### pattern ScreenX

`ScreenX` is a pattern to match on a [screenX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/screenX) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (ScreenX x e) = _
    handleMove _ = pure ()
```

### pattern MovementY

`MovementY` is a pattern to match on a [movementY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementY) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (MovementY y e) = _
    handleMove _ = pure ()
```

### pattern MovementX

`MovementX` is a pattern to match on a [movementX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (MovementX x e) = _
    handleMove _ = pure ()
```

### pattern PageY

`PageY` is a pattern to match on a [pageY](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageY) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (PageY y e) = _
    handleMove _ = pure ()
```

### pattern PageX

`PageX` is a pattern to match on a [pageX](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageX) position from a mouse event.

```haskell
someview = Div <| OnMouseMove handleMove
  where
    handleMove (PageX x e) = _
    handleMove _ = pure ()
```

### pattern Key0

`Key0` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `0`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key0 e) = _
    handle _ = pure ()
```

### pattern Key1

`Key1` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `1`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key1 e) = _
    handle _ = pure ()
```

### pattern Key2

`Key2` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `2`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key2 e) = _
    handle _ = pure ()
```

### pattern Key3

`Key3` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `3`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key3 e) = _
    handle _ = pure ()
```

### pattern Key4

`Key4` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `4`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key4 e) = _
    handle _ = pure ()
```

### pattern Key5

`Key5` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `5`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key5 e) = _
    handle _ = pure ()
```

### pattern Key6

`Key6` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `6`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key6 e) = _
    handle _ = pure ()
```

### pattern Key7

`Key7` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `7`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key7 e) = _
    handle _ = pure ()
```

### pattern Key8

`Key8` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `8`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key8 e) = _
    handle _ = pure ()
```

### pattern Key9

`Key9` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `9`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Key9 e) = _
    handle _ = pure ()
```

### pattern KeyA

`KeyA` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `A`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyA e) = _
    handle _ = pure ()
```

### pattern KeyB

`KeyB` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `B`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyB e) = _
    handle _ = pure ()
```

### pattern KeyC

`KeyC` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `C`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyC e) = _
    handle _ = pure ()
```

### pattern KeyD

`KeyD` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `D`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyD e) = _
    handle _ = pure ()
```

### pattern KeyE

`KeyE` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `E`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyE e) = _
    handle _ = pure ()
```

### pattern KeyF

`KeyF` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `F`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyF e) = _
    handle _ = pure ()
```

### pattern KeyG

`KeyG` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `G`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyG e) = _
    handle _ = pure ()
```

### pattern KeyH

`KeyH` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `H`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyH e) = _
    handle _ = pure ()
```

### pattern KeyI

`KeyI` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `I`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyI e) = _
    handle _ = pure ()
```

### pattern KeyJ

`KeyJ` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `J`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyJ e) = _
    handle _ = pure ()
```

### pattern KeyK

`KeyK` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `K`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyK e) = _
    handle _ = pure ()
```

### pattern KeyL

`KeyL` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `L`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyL e) = _
    handle _ = pure ()
```

### pattern KeyM

`KeyM` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `M`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyM e) = _
    handle _ = pure ()
```

### pattern KeyN

`KeyN` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `N`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyN e) = _
    handle _ = pure ()
```

### pattern KeyO

`KeyO` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `O`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyO e) = _
    handle _ = pure ()
```

### pattern KeyP

`KeyP` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `P`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyP e) = _
    handle _ = pure ()
```

### pattern KeyQ

`KeyQ` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `Q`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyQ e) = _
    handle _ = pure ()
```

### pattern KeyR

`KeyR` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `R`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyR e) = _
    handle _ = pure ()
```

### pattern KeyS

`KeyS` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `S`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyS e) = _
    handle _ = pure ()
```

### pattern KeyT

`KeyT` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `T`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyT e) = _
    handle _ = pure ()
```

### pattern KeyU

`KeyU` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `U`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyU e) = _
    handle _ = pure ()
```

### pattern KeyV

`KeyV` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `V`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyV e) = _
    handle _ = pure ()
```

### pattern KeyW

`KeyW` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `W`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyW e) = _
    handle _ = pure ()
```

### pattern KeyX

`KeyX` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `X`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyX e) = _
    handle _ = pure ()
```

### pattern KeyY

`KeyY` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `Y`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyY e) = _
    handle _ = pure ()
```

### pattern KeyZ

`KeyZ` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `Z`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (KeyZ e) = _
    handle _ = pure ()
```

### pattern OpenParenthesis

`OpenParenthesis` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `(`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (OpenParenthesis e) = _
    handle _ = pure ()
```

### pattern CloseParenthesis

`CloseParenthesis` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `)`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (CloseParenthesis e) = _
    handle _ = pure ()
```

### pattern Exclamation

`Exclamation` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `!`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Exclamation e) = _
    handle _ = pure ()
```

### pattern At

`At` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `@`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (At e) = _
    handle _ = pure ()
```

### pattern NumberSign

`NumberSign` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `#`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (NumberSign e) = _
    handle _ = pure ()
```

### pattern Dollar

`Dollar` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `$`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Dollar e) = _
    handle _ = pure ()
```

### pattern Percent

`Percent` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `%`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Percent e) = _
    handle _ = pure ()
```

### pattern Caret

`Caret` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `^`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Caret e) = _
    handle _ = pure ()
```

### pattern Ampersand

`Ampersand` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `&`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Ampersand e) = _
    handle _ = pure ()
```

### pattern Asterisk

`Asterisk` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `*`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Asterisk e) = _
    handle _ = pure ()
```

### pattern Underscore

`Underscore` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `_`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Underscore e) = _
    handle _ = pure ()
```

### pattern Plus

`Plus` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `+`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Plus e) = _
    handle _ = pure ()
```

### pattern VerticalBar

`VerticalBar` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `|`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (VerticalBar e) = _
    handle _ = pure ()
```

### pattern CurlyBracketLeft

`CurlyBracketLeft` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `{`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (CurlyBracketLeft e) = _
    handle _ = pure ()
```

### pattern CurlyBracketRight

`CurlyBracketRight` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `}`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (CurlyBracketRight e) = _
    handle _ = pure ()
```

### pattern QuestionMark

`QuestionMark` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `?`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (QuestionMark e) = _
    handle _ = pure ()
```

### pattern ForwardSlash

`ForwardSlash` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `/`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ForwardSlash e) = _
    handle _ = pure ()
```

### pattern Tilde

`Tilde` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `~`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Tilde e) = _
    handle _ = pure ()
```

### pattern Grave

`Grave` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `\``.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Grave e) = _
    handle _ = pure ()
```

### pattern Colon

`Colon` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `:`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Colon e) = _
    handle _ = pure ()
```

### pattern Semicolon

`Semicolon` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `;`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Semicolon e) = _
    handle _ = pure ()
```

### pattern Comma

`Comma` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `,`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Comma e) = _
    handle _ = pure ()
```

### pattern Period

`Period` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `.`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Period e) = _
    handle _ = pure ()
```

### pattern Quote

`Quote` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `'`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Quote e) = _
    handle _ = pure ()
```

### pattern DoubleQuote

`DoubleQuote` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `"`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (DoubleQuote e) = _
    handle _ = pure ()
```

### pattern BracketLeft

`BracketLeft` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `[`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (BracketLeft e) = _
    handle _ = pure ()
```

### pattern BracketRight

`BracketRight` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `]`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (BracketRight e) = _
    handle _ = pure ()
```

### pattern Backslash

`Backslash` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `\`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Backslash e) = _
    handle _ = pure ()
```

### pattern Minus

`Minus` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `-`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Minus e) = _
    handle _ = pure ()
```

### pattern Equal

`Equal` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `=`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Equal e) = _
    handle _ = pure ()
```

### pattern Alt

`Alt` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⎇ ` or `<Alt>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (Alt e) = _
    handle _ = pure ()
```

### pattern Alt

`Alt` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<AltGraph>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Alt e) = _
    handle _ = pure ()
```


### pattern CapsLock

`CapsLock` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⇪` or `<CapsLock>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (CapsLock e) = _
    handle _ = pure ()
```

### pattern Control

`Control` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⎈` or `✲` or `<Control>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (Control e) = _
    handle _ = pure ()
```

### pattern Fn

`Fn` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Fn>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (Fn e) = _
    handle _ = pure ()
```

### pattern FnLock

`FnLock` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<FnLock>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (FnLock e) = _
    handle _ = pure ()
```

### pattern Hyper

`Hyper` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Hyper>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Hyper e) = _
    handle _ = pure ()
```

### pattern OS

`OS` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⌘` or `◆` or `❖` or `<Meta>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (OS e) = _
    handle _ = pure ()
```

### pattern NumLock

`NumLock` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⇭` or `<NumLock>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (NumLock e) = _
    handle _ = pure ()
```

### pattern ScrollLock

`ScrollLock` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⤓` or `<ScrollLock>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (ScrollLock e) = _
    handle _ = pure ()
```

### pattern Shift

`Shift` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⇧` or `<Shift>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Shift e) = _
    handle _ = pure ()
```

### pattern Super

`Super` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Super>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Shift e) = _
    handle _ = pure ()
```

### pattern Enter

`Enter` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `↵` or `<Enter>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Enter e) = _
    handle _ = pure ()
```

### pattern Space

`Space` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Space>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Space e) = _
    handle _ = pure ()
```

### pattern Tab

`Tab` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `↹` or `<Tab>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Tab e) = _
    handle _ = pure ()
```

### pattern ArrowDown

`ArrowDown` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `↓` or `<ArrowDown>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ArrowDown e) = _
    handle _ = pure ()
```

### pattern ArrowLeft

`ArrowLeft` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `←` or `<ArrowLeft>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ArrowLeft e) = _
    handle _ = pure ()
```

### pattern ArrowRight

`ArrowRight` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `→` or `<ArrowRight>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ArrowRight e) = _
    handle _ = pure ()
```

### pattern ArrowUp

`ArrowUp` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `↑` or `<ArrowUp>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ArrowUp e) = _
    handle _ = pure ()
```

### pattern End

`End` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⇲` or `<End>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (End e) = _
    handle _ = pure ()
```

### pattern Home

`Home` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⇱` or `<Home>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (Home e) = _
    handle _ = pure ()
```

### pattern PageDown

`PageDown` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `PgDn` or `<PageDown>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (PageDown e) = _
    handle _ = pure ()
```

### pattern PageUp

`PageUp` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `PgUp` or `<PageUp>`.


```haskell
input = Input <| OnKeyDown handle
  where
    handle (PageUp e) = _
    handle _ = pure ()
```

### pattern Backspace

`Backspace` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Backspace>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Backspace e) = _
    handle _ = pure ()
```

### pattern Clear

`Clear` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Clear>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Clear e) = _
    handle _ = pure ()
```

### pattern Copy

`Copy` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Copy>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Copy e) = _
    handle _ = pure ()
```

### pattern CrSel

`CrSel` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<CrSel>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (CrSel e) = _
    handle _ = pure ()
```

### pattern Cut

`Cut` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Cut>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Cut e) = _
    handle _ = pure ()
```

### pattern Delete

`Delete` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Delete>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Delete e) = _
    handle _ = pure ()
```

### pattern EraseEof

`EraseEof` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<EraseEof>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (EraseEof e) = _
    handle _ = pure ()
```

### pattern ExSel

`ExSel` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<ExSel>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ExSel e) = _
    handle _ = pure ()
```

### pattern Insert

`Insert` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⎀` or `<Insert>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Insert e) = _
    handle _ = pure ()
```

### pattern Paste

`Paste` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Paste>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Paste e) = _
    handle _ = pure ()
```

### pattern Redo

`Redo` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Redo>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Redo e) = _
    handle _ = pure ()
```

### pattern Undo

`Undo` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⎌` or `<Undo>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Undo e) = _
    handle _ = pure ()
```

### pattern Accept

`Accept` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Accept>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Accept e) = _
    handle _ = pure ()
```

### pattern Again

`Again` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Again>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Again e) = _
    handle _ = pure ()
```

### pattern Attn

`Attn` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Attn>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Attn e) = _
    handle _ = pure ()
```

### pattern Cancel

`Cancel` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Cancel>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Cancel e) = _
    handle _ = pure ()
```

### pattern ContextMenu

`ContextMenu` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<ContextMenu>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ContextMenu e) = _
    handle _ = pure ()
```

### pattern Escape

`Escape` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `⎋` or `<escape>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Escape e) = _
    handle _ = pure ()
```

### pattern Execute

`Execute` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Execute>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Execute e) = _
    handle _ = pure ()
```

### pattern Find

`Find` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Find>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Find e) = _
    handle _ = pure ()
```

### pattern Finish

`Finish` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Finish>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Finish e) = _
    handle _ = pure ()
```

### pattern Help

`Help` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Help>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Help e) = _
    handle _ = pure ()
```

### pattern Pause

`Pause` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Pause>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Pause e) = _
    handle _ = pure ()
```

### pattern Play

`Play` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Play>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Play e) = _
    handle _ = pure ()
```

### pattern Props

`Props` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Props>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Props e) = _
    handle _ = pure ()
```

### pattern Select

`Select` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<Select>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (Select e) = _
    handle _ = pure ()
```

### pattern ZoomIn

`ZoomIn` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<ZoomIn>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ZoomIn e) = _
    handle _ = pure ()
```

### pattern ZoomOut

`ZoomOut` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<ZoomOut>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (ZoomOut e) = _
    handle _ = pure ()
```

### pattern F1

`F1` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F1>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F1 e) = _
    handle _ = pure ()
```

### pattern F2

`F2` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F2>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F2 e) = _
    handle _ = pure ()
```

### pattern F3

`F3` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F3>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F3 e) = _
    handle _ = pure ()
```

### pattern F4

`F4` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F4>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F4 e) = _
    handle _ = pure ()
```

### pattern F5

`F5` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F5>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F5 e) = _
    handle _ = pure ()
```

### pattern F6

`F6` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F6>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F6 e) = _
    handle _ = pure ()
```

### pattern F7

`F7` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F7>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F7 e) = _
    handle _ = pure ()
```

### pattern F8

`F8` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F8>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F8 e) = _
    handle _ = pure ()
```

### pattern F9

`F9` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F9>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F9 e) = _
    handle _ = pure ()
```

### pattern F10

`F10` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F10>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F10 e) = _
    handle _ = pure ()
```

### pattern F11

`F11` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F11>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F11 e) = _
    handle _ = pure ()
```

### pattern F12

`F12` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F12>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F12 e) = _
    handle _ = pure ()
```

### pattern F13

`F13` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F13>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F13 e) = _
    handle _ = pure ()
```

### pattern F14

`F14` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F14>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F14 e) = _
    handle _ = pure ()
```

### pattern F15

`F15` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F15>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F15 e) = _
    handle _ = pure ()
```

### pattern F16

`F16` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F16>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F16 e) = _
    handle _ = pure ()
```

### pattern F17

`F17` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F17>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F17 e) = _
    handle _ = pure ()
```

### pattern F18

`F18` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F18>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F18 e) = _
    handle _ = pure ()
```

### pattern F19

`F19` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F19>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F19 e) = _
    handle _ = pure ()
```

### pattern F20

`F20` is a pattern to match on a [key](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key) of `<F20>`.

```haskell
input = Input <| OnKeyDown handle
  where
    handle (F20 e) = _
    handle _ = pure ()
```