# pure-state

This package is an experiment in monadic/stateful views.

An example of a stateful counter:

```haskell
counter = runPureWithIO (0 :: Int) $ \ref -> do
  n <- State.get
  let button f cs = 
        Button <| OnClick (\_ -> modifyWith ref f) |> cs
  Div =<||>
    [ button pred [ "-1" ]
    , text n
    , button succ [ "+1" ]
    ]
```

See [state-management](/blog/2019/1/8/state-management) for an overview of the ways in which state is managed in Pure.

[Runners](#runPureIO)
[Ref methods](#ref)
[Combinators](#combinators)
[Injector](#injectPure)

## Pure.State

`Pure.State` implements a state monad transformer than is run as a `View`.

Four variants of state runners are exported.

### `runPureIO` 

Embed a stateful view.

```haskell
runPureIO :: (Typeable s) => s -> PureM s IO View -> View
```

### `runPure`

Embed a stateful view transformer when given a method to run the wrapped monad in IO.

```haskell
runPure :: (Typeable s, Typeable m) => (forall a. m a -> IO a) -> s -> PureM s m View -> View
```

### `runPureWith`

Short-hand to avoid a call to ref:

```haskell
runPureWith f s m == runPure f s (ref >>= m)
```

### `runPureWithIO`

Short-hand to avoid a call to ref:

```haskll
runPureWithIO s m == runPureIO s (ref >>= m)
```

### `ref`

Get a handle on a pseudo-mutable reference to the view's state. The state reference is useful when a generated view needs to update state from within event handlers.

This is required because standard `get`, `put`, and `modify` only work within the view producer, not the produced view.

```haskell
runPureIO (0 :: Int) $ do
  r <- ref
  n <- get
  Div =<||>
    [ text n
    , Button <| OnClick (const (modifyWith r succ))  |> 
      [ "+1" ]
    ]
```

### `getWith` 

Effectfully read a state from a reference. This is used in event handlers to view state and avoid forcing unnecessary reconciliations.

Note, in the view below, because the `n` changes every time the state is updated, the `Button` will have to be updated every time the state is updated:

```haskell
runPureIO (0 :: Int) $ do
  r <- ref
  n <- get
  Div =<||>
    [ text n
    , Button <| OnClick (const (putWith r (n + 1))) |> 
      [ "+1" ]
    ]
```

But this equivalent version doesn't have the same problem because the `ref` is static:

```haskell
runPureIO (0 :: Int) $ do
  r <- ref
  n <- get
  Div =<||>
    [ text n
    , Button <| OnClick (const (do { n' <- getWith r; putWith r (n' + 1) })) |> 
      [ "+1" ]
    ]
```

### `putWith`

Effectfully update a state from a reference. This is used in event handlers to update state from IO.

```haskell
runPureIO (0 :: Int) $ do
  r <- ref
  n <- get
  Div =<||>
    [ text n
    , Button <| OnClick (const (putWith r 0)) |> [ "Reset" ]
    , Button <| OnClick (const (modifyWith r (+1))) |> [ "+1" ]
    ]
```

### `modifyWith`

Short-hand for `getWith + putWith`:

```haskell
modifyWith ref f = getWith ref >>= \x -> putWith ref (f x)
```

### Combinators

`Pure.State` comes with variants of the combinators from [pure-core](/doc/pure-core/0.7.0.0) for lifting view construction into an applicative or monadic context.

For instance, instead of:

```haskell
pure $ Div <||> [ "Some Content" ]
```

You can lift the `View` automatically with:

```haskell
Div =<||> [ "Some Content" ]
```

Three combinators for lifting views into an applicative:

```haskell
<|    == =<|     :: (Applicative f, ToView b) => a -> (a -> b) -> f View
<||>  == =<||>   :: (Applicative f, ToView a, HasChildren a) => a -> [View] -> f View
<||#> == =<||#>  :: (Applicative f, ToView a, HasKeyedChildren a) => a -> [(Int,View)] -> f View
```

Two combinatora for applicative children in un-keyed and keyed variants:

```haskell
|>  == |>=       :: (Applicative m, HasChildren a) => (a -> a) -> [m View] -> a -> m a)
|#> == |#>=      :: (Applicative m, HasKeyedChildren a) => (a -> a) -> [m (Int,View)] -> a -> m a 
```

Two combinators for applicative views with applicative children in un-keyed and keyed variants:

```haskell
=<||>=           :: (Applicative m, ToView a, HasChildren a) => a -> [m View] -> m View
=<||#>=          :: (Applicative m, ToView a, HasKeyedChildren a) => a -> [m (Int,View)] -> m View
```

### `injectPure`

This is a short-hand for `Pure.State`-oriented applications:

```haskell
injectPure :: (IsNode e, Typeable s) => e -> s -> PureM s IO View -> IO ()
injectPure e s p = inject e (runPureIO s p)
```

