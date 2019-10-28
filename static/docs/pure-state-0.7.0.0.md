# pure-state

This package is an experiment in monadic stateful views. In some cases monadic views are more convenient and easier to think about and implement - this is especially true for dynamic stateful views like forms.

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

## Pure.State

`Pure.State` implements a state monad transformer than is run as a `View`.

### type PureM

`PureM` is a state monad that gets evaluated as a `Component`. `PureM` is always at the bottom of a transformer stack, like `IO`.

```haskell
newtype PureM s a = PureM { unPureM :: Reader.ReaderT (SRef s) IO a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus,MonadIO
           ,MonadFix,MonadCatch,MonadThrow,MonadMask
           )
```

> Note that the `SRef` is a mutable reference, so the core monad transformer is `ReaderT` instead of `StateT`. The state is required to be mutable to be compatible with evaluation as a `Component`.

### type SRef

A reference to some mutable state `s` with an extensible write effect that enables implicitly reactive views produced in monadic contexts.

The `SRef` serves a dual purpose:

  1. Code that uses the state to produce a view must be able to modify the state without causing re-evaluation of the view-producing code which could cause possible unintended looping of the view-producing code.

  2. Produced views must be able to inject changes back into the reference and force re-evaluation of the view-producing code.

```haskell
data SRef s = SRef (IORef s) (s -> IO ())
```

### class MonadSRef

`MonadSRef` represents a class of contexts with access to an `SRef`.

```haskell
class MonadIO m => MonadSRef s m | m -> s where
  sref :: m (SRef s)
  getState :: m s
  setState :: s -> m ()
  modifyState :: (s -> s) -> m ()
  {-# MINIMAL sref #-}
```

The core instance is for `PureM`.

```haskell
instance MonadSRef s (PureM s) where
  sref = PureM Reader.ask
```

A lifted instance for transformers of `PureM` exists.

```haskell
instance (MonadTrans t, MonadIO (t m), MonadSRef s m) => MonadSRef s (t m) where
  sref = lift sref
```

And, given a `MonadSRef s m + MonadIO m`, there exists a `MonadState s m` instance.

```haskell
instance (MonadIO m, MonadSRef s m) => MonadState s m where
  get = getState
  put = setState
```

### sref

Get a handle on a pseudo-mutable reference to the view's state. The state reference is useful when a generated view needs to update state from within event handlers.

```haskell
sref :: MonadSRef s m => m (SRef s)
```

This is required because standard `get`, `put`, and `modify` only work within the view producer, not the produced view.

```haskell
incrementer = do
  r <- sref
  Button =<| OnClick (const (modifyWith r succ)) |>
    [ "+1" ]
```

### getState

Retrieve the current state of a `PureM` view from within the view-producing code. This method does not work inside the produced view, itself.

```haskell
getState :: MonadSRef s m => m s
```

```haskell
current :: ToTxt a => PureM a View
current = text <$> getState
```

### setState

Set the state of an SRef in a context containing access to the SRef. This will generally be in view-producing code and the update will be available to any code executed after this modification, but will not force any previously executed code to re-evaluate, unlike `writeSRef` which queues an update for re-evaluation.

```haskell
setState :: MonadSRef s m => s -> m ()
```

### modifyState

Modify the state of an SRef in a context containing access to the SRef. This will generally be in view-producing code and the update will be available to any code executed after this modification, but will not force any previously executed code to re-evaluate, unlike `modifySRef` which queues an update for re-evaluation.

```haskell
modifyState :: MonadSRef s m => (s -> s) -> m ()
```


### push

Sometimes, it is desirable to have `put` cause the view producing code to re-evaluate. To force re-evaluation of a view-producer from within the view-producer itself, there is `push`, a shorthand method.

```haskell
push :: MonadSRef s m => s -> m ()
push s = sref >>= \ref -> writeSRef ref s
```

### pushes

Similar to `push` as an extension of `put`, there is `pushes` as an extension to `modify`.

```haskell
pushes :: MonadSRef s m => (s -> s) -> m ()
pushes f = sref >>= \ref -> readSRef ref >>= \s -> writeSRef ref (f s)
```

### data Reactive

`Reactive` is a configuration of the reactivity of a `PureM` view producer.

```haskell
data Reactive = Reactive
  { onState :: Bool
  , onPure  :: Bool
  }
```

To specify that a view should be re-evaluated if the initial (injected) state changes, use `onState = True`. To specify that a view should be re-evaluated if the view producer changes, use `onPure = True`. There is a default non-reactive instance that specifies that the view should not respond to any outside changes.

```haskell
instance Default Reactive where
  def = Reactive False False
```

### runPureWith

The core evaluation method is `runPureWith` that takes a `Reactive`, an initial state `s`, and a stateful view producer `PureM s View`.

```haskell
runPureWith :: Reactive -> s -> PureM s View -> View
```

### runPure

`runPure` is a shorthand for non-dynamic `PureM` evaluation.

```haskell
runPure :: s -> PureM s View -> View
runPure = runPureWith def
```

### runPureDyn

`runPureDyn` is a shorthand for dynamic `PureM` evaluation.

```haskell
runPureDyn :: s -> PureM s View -> View
runPureDyn = runPureWith Reactive
  { onState = True, onPure = True }
```

### liftPure

`liftPure` will lift a `PureM s a` into another monad `MonadIO m => m a`, but requires access to the `SRef s` to do so.

```haskell
liftPure :: MonadIO m => SRef s -> PureM s a -> m a
```

### embedPure

`embedPure` will embed a lifted `PureM s a` into a runnable context, but will not run it directly as `liftPure` does.

```haskell
embedPure :: (MonadSRef s m, MonadIO m, MonadIO n) => PureM s a -> m (n a)
embedPure m = do
  s <- sref
  pure ( liftPure s m )
```

This is used to construct a callback that can be run from another `IO`-capable context, like an event listener.

```haskell
enumeration :: (ToTxt a, Enum a) => PureM a View
enumeration = do
  inc <- embedPure (modify pred)
  dec <- embedPure (modify succ)
  n <- get
  Div =<||>
    [ Button <| OnClick (const inc) |> [ "+1" ]
    , text n
    , Button <| OnClick (const dec) |> [ "-1" ]
    ]
```

### readSRef

Effectfully read a state from a reference. This is used in event handlers to view state and avoid forcing unnecessary reconciliations.

```haskell
readSRef :: MonadIO m => SRef s -> m s
```

Note, in the view below, because the `n` changes every time the state is updated, the `Button` will have to be updated every time the state is updated.

```haskell
counter = do
  r <- ref
  n <- get
  Div =<||>
    [ text n
    , Button <| OnClick (const (writeSRef r (n + 1))) |>
      [ "+1" ]
    ]
```

But this equivalent version doesn't have the same problem because the `r :: SRef _` is static.

```haskell
counter = do
  r <- sref
  n <- get
  Div =<||>
    [ text n
    , Button <| OnClick (const (readSRef r >>= writeSRef r . succ)) |>
      [ "+1" ]
    ]
```

### writeSRef

Write the current state of an SRef in a MonadIO context and force re-evaluation of the view managing the SRef.

```haskell
writeSRef :: MonadIO m => SRef s -> s -> m ()
```

Choose `writeSRef` for effect handlers within `View`s and `writeState` or `put` within view-producing code, like `PureM`.

> NOTE: Using `writeSRef` within view-producing code is likely to cause unintended looping.

```haskell
reset ref = Button <| OnClick (const $ writeSRef ref 0)
```

### modifySRef

Modify the current state of an SRef in a MonadIO context and force re-evaluation of the view managing the SRef.

```haskell
modifySRef :: MonadIO m => SRef s -> (s -> s) -> m ()
```

Choose `modifySRef` for effect handlers within `View`s and `modifyState` or `modify` within view-producing code, like `PureM`.

> NOTE: Using `modifySRef` within view-producing code is likely to cause unintended looping.

```haskell
increment ref = Button <| OnClick (const $ modifySRef ref succ)
```

### Combinators

`Pure.State` comes with variants of the combinators from [pure-core](/doc/pure-core) for lifting view construction into an applicative or monadic context.

> In general, I suggest not building view this way, as it can be confusing. Instead, build views in monadic code and then piece them together after binding in pure constructions. These combinators also have not been well tested for performance w.r.t. reconciliation.

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