# Components

Components enable stateful dynamic views that execute asynchronously.

Dynamic views are reactive to internal and external events.

```haskell
counterView :: IO () -> IO () -> Int -> View
counterView decrement increment current =
  Div <| Class "counter" |>
    [ Button <| Class "action" . OnClick (const decrement) |> [ "-" ]
    , text current
    , Button <| Class "action" . OnClick (const increment) |> [ "+" ]
    ]
```

To inject changes into `counterView`, the counter state is localized to a component.

```haskell
counter :: Int -> View
counter = Component $ \self ->
  let
    update :: (Int -> Int) -> IO ()
    update f = modify_ self $ \_ i -> f i
  in
    def
      { construct = ask
      , render = \_ current -> counterView (update pred) (update succ) current
      }
```

This component can be injected as a stand-alone application.

```haskell
main = inject body (counter 0)
```

After the increment or decrement buttons are clicked, the component will respond to changes in state by updating the DOM. This is often called reconciliation. In this case, the only required reconciliation is with the displayed current value inside `counterView` - a quick diff.

See the [Server Tutorial](/tut/servers) to learn how the same `Component` abstraction is used to write generic hierarchical computational contexts, including servers and services.