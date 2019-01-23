# pure-core

This package implements core `View` types, including manipulation functions and pattern synonyms.

## Pure.Data.View

`Pure.Data.View` exports the core `View`, `Listener`, `Features`, `Component`, and `Ref` types, as well as some core component methods and the `Pure` class.

For HTML `View` construction, see [pure-html](/doc/pure-html/0.7.0.0).

For SVG `View` construction, see [pure-svg](/doc/pure-svg/0.7.0.0).

### The Pure class and the View pattern

The `Pure` class enriches data with a rendering method:

```haskell
class Pure a where
  view :: a -> View
```

The `View` pattern will construct a `View` from any value implementing the `Pure` class:

```haskell
pattern View :: Pure a => a -> View
```

This encourages modularization and the use of data-oriented views:

```haskell
newtype Username = Username { username :: Txt }
instance Pure Username where
  view (Username un) = text un

users :: [User] -> [View]
users = fmap View
```

### `get`

`get` view the current state of a component from a reference to a component without blocking.

```haskell
get :: Ref props state -> IO state
```

### `ask`

`ask` extracts the current properties of a component from a reference to a component without blocking.

```haskell
ask :: Ref props state -> IO props
```

### `modify`

`modify` enqueues a pure update to a component's state. The callback passed to `modify` will be run in the component's next update cycle. `modify` returns `True` if there are no component updates pending.

```haskell
modify :: Ref props state -> (props -> state -> state) -> IO Bool
```

> NOTE: Updates are batched together. As a result, the state returned by a `modify` may never be rendered. To avoid this, use nested calls to `modifyM` which guarantees that the updates are run in separate update cycles.

### `modifyM`

`modifyM` is a more general versions of `modify` that allows you to effectfully modify the state of a component as well as return a callback that is run after the state has been updated. `modifyM` returns `True` if there are no component updates pending.

```haskell
modifyM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
```

> NOTE: Updates are batched together.
> 
> Calling `get` in the callback returned from an update function in `modifyM` is not guaranteed to see the state returned from the update function - it is, however, guaranteed to see that state or a newer state. 
>
> Due to batching, it is possible to make changes to the state of a component that will never be rendered. To avoid this scenario, you can nest calls to `modifyM` to throttle updates:
>
> ```haskell
> modifyM ref $ \props state -> do
>   state' <- someStateUpdate state
>   return (state',modifyM_ ref $ \props state -> do
>     state' <- someThrottledStateUpdate state
>     return (state',return ())
>     )
> ```
>
> Nested calls to `modifyM` are guaranteed not to run in the same component update cycle.

## Pure.Data.View.Patterns

`Pure.Data.View.Patterns` exports patterns, classes, and combinators for [View](#View) construction.

### `Component`

A view pattern for construction of components.

```haskell
pattern Component :: (Ref props state -> Comp props state) -> props -> View
```

```haskell
user :: User -> View
user = Component $ \self -> 
  def
    { ...
    }
```

### `Null`

A null view pattern for conditonal rendering of views.

```haskell
activeUser :: Maybe Username -> View
activeUser Nothing = Null
activeUser (Just u) = {...}
```

### `Raw`

A pattern for unsafe construction of an HTML view from a text value. 

> WARNING: `Raw` does not sanitize its input!

```haskell
rawH1 = Raw "<h1>Raw H1</h1>"
```

### `Keyed`

A pattern for creating a keyed variant of an `HTML` view or an `SVG` view. Use `(|#>)` or `(<||#>)` to add keyed children to a keyed view.

```haskell
keyedDiv :: View
keyedDiv = Keyed Div
```

### `Class`

A pattern for adding a class to a view. Note that empty class strings are filtered out during rendering.

```haskell
myView = Div <| Class "someClass"
```

### `Style`

A pattern for adding a style to a view.

```haskell
myView = Div <| Style "width" "100%"
```

### `Property`

A pattern for adding a property to a view.

```haskell
myView val = Input <| Property "value" val
```

### `Attribute`

A pattern for adding an attribtue to a view.

```haskell
myView = Input <| Attribute "id" "myView"
```

### `<|`

Apply the modification on the right before calling `toView`. If the argument on the left is a `View`, it is equivalent to calling `id`. If it is an instance of `Pure`, it is equivalent to wrapping with the `View` pattern.

```haskell
```

### `|>`

Specify children after applying properties.

```haskell
Div <| Class "greeting" |>
  [ Span <| Class "hello" |> [ "Hello" ]
  , Span <| Class "world" |> [ ", World!" ]
  ]
```

### `<||>`

Specify children.

```haskell
Div <||>
  [ Span <||> [ "Hello" ]
  , Span <||> [ ", World!" ]
  ]
```

### `|#>`

Specify keyed children after applying properties. Must be paired with `Keyed` or used with views supporting `HasKeyedChildren`.

```haskell
users us =
  Keyed Div <| Class "users" |#>
    [ (hash u, View u)
    | u <- us
    ]
```

### `<||#>`

Specify keyed children. Must be paired with `Keyed` or used on views supported `HasKeyedChildren`.

```haskell
users us =
  Keyed Div <||#> 
    [ (hash u, View u)
    | u <- us
    ]
```

### `text`

Inject a textual value into a `View`.

```haskell
text :: IsTxt a => a -> View
```

### `txt`

A specialized version of `text` for `Txt`.

```haskell
txt :: Txt -> View
```
