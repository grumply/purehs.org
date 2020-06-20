*pure-events* implements patterns for working with browser-based events, like clicks, transitions, mouse moves, touches, and key presses, and input by
taking a unified approach to event listeners with a core `On` pattern for any particular event as well as an `OnWith` pattern when configuration is required for that event.

The patterns are defined in two tiers.

The first tier are the generic patterns, `On`, `OnWith`, `OnDoc`, `OnDocWith`, `OnWin`, and `OnWinWith`. Since listeners are concomitant with views, being defined as properties of the view, they may create host element, window, and document listeners that will be cleaned up when the view is removed. That is, window- and document-targeted event listeners don't have to be side-loaded and separately managed - they can be created and removed automatically when the view is mounted or unmounted, just like host-targeted listeners.

The second tier are the targeted patterns, e.g. `OnClick` and `OnClickWith`, that are defined in terms of `On` and `OnWith`.

<div class="info">
All listener patterns in this library are constructor-only; these patterns cannot be used for matching. When using these patterns for matching, an error will be thrown suggesting an alternative approach.

Since GHC doesn't support constructor-only patterns, it is necessary to live with this approach to keep the aesthetics of the framework consistent.

The following example ***will not*** work. 

```haskell
getClickListener :: View -> Maybe (Evt -> IO ())
getClickListener (OnClick f Button) = Just f
getClickListener _ = Nothing
```

The following error will be produced:

```
The Listener pattern does not support matching, 
only construction. For pattern matching on 
listeners, use the Listeners pattern.
```
</div>
