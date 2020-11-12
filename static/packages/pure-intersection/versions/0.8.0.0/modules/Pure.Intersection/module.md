## data Observer

The [IntersectionObserver](https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API) component. Uses a standard Pure.hs property-based implementation.

```haskell
data Observer = Observer_
  { as         :: Features -> [View] -> View
  , features   :: Features
  , children   :: [View]
  , root       :: JSV
  , rootMargin :: Txt
  , threshold  :: [Double]
  , action     :: [Intersection] -> IO ()
  , disable    :: Bool
  }
```

<div class="hide">
Configuration properties are available 
  
  * [As](Pure.Intersection/pattern%20As) for changing the observer element
  * [Root](Pure.Intersection/pattern%20Root) for retargeting the observer root, changing the viewport for intersection calculations
  * [RootMargin](Pure.Intersection/pattern%20RootMargin) for setting a root margin to grow or shrink the viewport before intersection calculations
  * [Threshold](Pure.Intersection/pattern%20Threshold) for configuring ratio-visible thresholds 
  * [Action](Pure.Intersection/pattern%20Action) for setting an action with access to intersection events
  * [Disable](Pure.Intersection/pattern%20Disable) for disabling the observer 
  
Note that instances for [HasFeatures](/packages/pure-core/latest/Pure.Data.View.Patterns/class%20HasFeatures) and [HasChildren](/packages/pure-core/latest/Pure.Data.View.patterns/class%20HasChildren) allows for using [<|](/packages/pure-core/latest/Pure.Data.View.Patterns/%3C%7C), [<||>](/packages/pure-core/latest/Pure.Data.View.Patterns/%3C%7C%7C%3E), and [|>](/packages/pure-core/latest/Pure.Data.View.Patterns/%7C%3E) with a value of type `Observer`.

```haskell
import Pure hiding (any)
import Pure.Intersection as O

main = inject body test

test = 
  Div <||>
    [ Div <| Height (800px)
    , Observer def <| O.Threshold [0] . O.Action act |>
        [ Div <| Height (20px) . BackgroundColor green
        ]
    ]

act :: [Intersection] -> IO ()
act is
  | any intersecting is = print "Intersected"
  | otherwise           = print "Not Intersected"
```

For use in contexts where `RootMargin` does not work, like cross-origin iframes, it can be necessary to manipulate the position context. The above example is a case where this position context would have been necessary for me to make the example a live example, but to do so would be didactically detrimental since most use-cases are not within iframes. To see an example, check out [pure-stream](/packages/pure-stream/latest), where [frameStepper](/packages/pure-stream/latest/Pure.Stream/frameStepper) is intended to resolve the issue in iframes for streaming views.

For an detailed explanation, see the spec: https://w3c.github.io/IntersectionObserver/#dom-intersectionobserver-rootmargin 
</div>

## data Intersection

The type of intersection events. Of particular import is the `intersecting` value, which covers most use-cases.

See the documentation at MDN for [IntersectionObserverEntry](https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverEntry) for a deep dive on each of the fields.

```haskell
data Intersection = Intersection
  { bounds       :: Rect
  , ratio        :: Double
  , intersection :: Rect
  , intersecting :: Bool
  , rootBounds   :: Maybe Rect
  , target       :: JSV
  , time         :: Double
  }
```

## pattern Observer

This pattern is a convenience for fixing the type of a defaulted [Observer](Pure.Intersection/data%20Observer). 

```haskell
pattern Observer :: Observer -> Observer
pattern Observer o = o
```

<div class="hide">

The `Observer` pattern is largely for aesthetics to fit with the style of views generally written in Pure.hs, and can be replaced by a `TypeApplication` specifying the type of `def` to [Observer](Pure.Intersection/data%20Observer).

```haskell
myView someAction target = 
  Observer def <| Threshold [0] . Action someAction |> 
    [ target ]
```

Is equivalent to

```haskell
myView someAction target =
  def @Observer <| Threshold [0] . Action someAction |> 
    [ target ]
```

</div>

## pattern As

Set the host view of an intersection observer. By default, the intersection observer will be attached to a `<div>`.

```haskell
span :: Features -> [View] -> View
span fs cs = Span & Features fs & Children cs

observed :: View
observed = Observer def <| As span ...
```

## pattern Root

Set the root of an intersection observer. The root of an intersection observer defines the viewport for which intersections are calculated. If nothing is supplied, the root defaults to the browser's viewport. This must be an ancestor of the intersection observer.

```haskell
observed :: JSV -> View
observed ancestor = Observer def <| Root ancestor ...
```

## pattern RootMargin

The property pattern for setting a root margin. Root margin allows for shrinking or growing the viewport for which intersections are calculated. The format is similar to that of the margin property, but supports only percentage and pixel values. Multi-property shorthand is supported, as well.
 
```haskell
observed :: View
observed = Observer def <| RootMargin (200px) ...

multi :: View
multi = Observer def <| RootMargin "20px 40px" ...
```

## pattern Threshold

Set the thresholds, as a list of ratios, for which the intersection observer action, set with [Action](Pure.Intersection/pattern%20Action), should be called. The threshold is a ratio of the visibility of the observed target within the root viewport.

If your intention is to fire a callback as soon as the element starts to enter the viewport, simply use `Threshold [0]`. 

<div class="info">
Note that the callbacks fire when the element is entering or leaving the viewport.
</div>

<div class="warn">
If no threshold is supplied, the observer fires the action callback once when mounted and then never again.
</div>

```haskell
-- Fire the observer's callback when the target first becomes visible in the 
-- viewport, when it is half visible, and when it is fully visible.
observed :: View
observed = Observer def <| Threshold [0,0.5,1] ...
```

## pattern Action

Set the action to run when an intersection threshold has been met. The list of intersections is supplied to the callback. 

<div class="info">
In practice, this list only contains a single intersection, but a list is necessary to correctly support the standard.
</div>

```haskell
observed :: View
observed = Observer def <| Action myAction ...

myAction :: [Intersection] -> IO ()
myAction is
  | any intersecting is = print "Intersecting"
  | otherwise           = print "Not Intersecting"
```

## pattern Disable

Set the disable flag of the observer to stop observing intersections.

```haskell
observed :: Bool -> View
observed disable = Observer def <| Disable disable ...
```