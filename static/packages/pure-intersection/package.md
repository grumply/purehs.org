A simple wrapper over the [IntersectionObserver API](https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API) which allows for 

> [observing] changes in the intersection of a target element with an ancestor element.

Intersection observers solve the issue of poor performance of scroll listeners with `getBoundingClientRect()` calls by tying into an internal implementation of similar functionality at a lower level within the browser's DOM management interface.

Below is an example of a simple component for lazy loading by way of an intersection observer. Wrap any `View` with `lazy` to make it render lazily.

```haskell
import Pure.Elm hiding (Action,any,lazy,action)
import Pure.Intersection

main = inject body test

test =
  Div <||>
    [ Div <| Height (120vh)
    , lazy "Now you see me."
    ]

lazy :: View -> View 
lazy = run app
  where
    app = App [] [] [] (pure mdl) upon view
    
    -- Default to not loaded
    mdl = False
    
    -- When any message is received from the observer, 
    -- set the state to `True`.
    upon _ _ _ = pure True

    view someView loaded = 
      Observer def <| Threshold [0] . Action act |>
        [ if loaded then someView else Null ]

    -- Send a unital command when the lazy 
    -- wrapper has intersected the viewport.
    act is
      | any intersecting is = command ()
      | otherwise           = pure ()
```

<div class="note">

For a higher-level API that implements lazy-loading of finite and infinite streaming views, check out [pure-stream](/packages/pure-stream/latest).

</div>