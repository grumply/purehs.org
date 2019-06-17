# Components

Components enable dynamic views that execute asynchronously.

Dynamic views can permit interaction with the user:

```haskell
counterView :: IO () -> IO () -> Int -> View
counterView decrement increment current =
  Div <| Class "counter" |>
    [ Button <| Class "action" . OnClick (const decrement) |> [ "-" ]
    , text current
    , Button <| Class "action" . OnClick (const increment) |> [ "+" ]
    ]
```

But state for a dynamic view must be localized.

```haskell
counter :: Int -> View
counter = Component $ \self ->
  let 
    update :: (Int -> Int) -> IO ()
    update f = modify_ self $ \_ -> f
  in 
    def 
      { construct = ask
      , render = \_ current -> counterView (update pred) (update succ) current
      }
```

This component can be injected as a stand-alone application:


```haskell
main = inject body (counter 0)
```

After the increment or decrement buttons are clicked, the component will respond
to changes in state by updating the DOM. This is called reconciliation. In this 
case, the only required reconciliation is with the displayed current value 
inside `counterView` - a quick diff. 

Components have lifecycle methods that trigger at different points in the life
of the component:

```haskell
data Comp props state = Comp
  { construct    :: IO state
  , mount        :: state -> IO state
  , executing    :: IO ()
  , mounted      :: IO ()
  , receive      :: props -> state -> IO state
  , force        :: props -> state -> IO Bool
  , update       :: props -> state -> IO ()
  , render       :: props -> state -> View
  , updated      :: props -> state -> IO ()
  , unmounted    :: IO ()
  }
```

Lifecycle method properties. `#` corresponds to number of times the lifecycle
method will be evaluated.

| Lifecycle  |  # |                     Trigger |            Arguments |        Returns |
| ---------- | -- | --------------------------- | -------------------- | -------------- |
| construct  |  1 |                     startup |                      |  initial state |
| mount      |  1 |         before first render |        current state |      new state |
| executing  |  1 |            component forked |                      |                |
| mounted    |  1 |                view mounted |                      |                |
| receive    | 0+ |              new properties | new props, old state |      new state |
| force      | 0+ |     new properties or state | new props, new state | re-render flag |
| update     | 0+ |   before view is reconciled | new props, new state |                |
| render     | 1+ | first render and every diff | new props, new state |           view |
| updated    | 0+ |             view reconciled | old props, old state |                |
| unmounted  |  1 |                    shutdown |                      |                |

> Note that `render` happens before executing the first time, but, subsequently,
> after `update` and before `updated`. For nested components, `mount` is 
> top-down, `mounted` is bottom-up, and `unmounted` is top-down. For `receive`,
> `force`, `update`, and `updated`, no order can be assumed relative to other
> components, as components are evaluated independently of each other.


