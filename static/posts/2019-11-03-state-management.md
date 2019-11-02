# State management in Pure

Pure implements several approaches to state management:

* [1](#pure-core) [Pure components](/doc/pure-core) have internal state that can be modified locally and non-locally via a reference.

* [2](#pure-state) [pure-state](/doc/pure-state) implements monadic state management.

* [3](#excelsior) [excelsior](/doc/excelsior) implements redux-style global state management using dynamic messaging.

* [4](#pure-elm) [pure-elm](/doc/pure-elm) implements [The Elm Architecture](https://guide.elm-lang.org/architecture/).

## pure-core

Standard `Component` state is a staple of Pure and is defined as a core `View` type.

All approaches to state management derive from the core `Component` facility, including [pure-state](/doc/pure-state), [pure-elm](/doc/pure-elm), and [excelsior](/doc/excelsior).

We initialize the state within the `construct` field and we write component-local (let-bound) update functions. Note that these let-bound update functions may be freely passed throught your application and, when used, return `True` if the component is still alive.

```haskell
counter = flip ComponentIO () $ \self ->
  let
    increment = modify_ self $ \_ -> succ
    decrement = modify_ self $ \_ -> pred
  in
    def
      { construct = return (0 :: Int)
      , render = \_ n ->
        Div <||>
          [ Button <| OnClick (const increment) |>
            [ "Increment" ]
          , text n
          , Button <| OnClick (const decrement) |>
            [ "Decrement" ]
          ]
      }

main = inject body counter
```

[Many other lifecycle methods](/doc/pure-core/0.7.0.0/Pure.Data.View/data%20Comp) exist for `Components`.

## pure-state

[pure-state](/doc/pure-state) implements a stateful `View`-builder monad. Due to the monadic nature of `pure-state`, this is especially useful for forms!

```haskell
counter = runPure (0 :: Int) $ do
  n   <- get
  inc <- embedPure (modify pred)
  dec <- embedPure (modify succ)
  pure $
    Div <||>
      [ Button <| OnClick (const inc) |>
        [ "Increment" ]
      , text n
      , Button <| OnClick (const dec) |>
        [ "Decrement" ]
      ]

main = inject body counter
```

## excelsior

`excelsior` uses a dynamically-typed approach to state management by allowing ADTs of commands to dictate actions to which reducers and reducer transformers may respond.

Reducers are pure functions that transform state:

```haskell
type Reducer state command =
  command -> state -> state

myReducer :: Reducer MyState MyCommand
myReducer SomeCommand (MyState x) =
  MyState (someStateModifier x)
```

Middlewares are reducer transformers:

```haskell
type Middleware state command =
  (command -> state -> IO state) ->
  (command -> state -> IO state)

myMiddleware :: Middleware MyState MyCommand
myMiddleware continue SomeCommand (MyState x)
  | conditional x =
    continue OtherCommand (MyState x)
  | otherwise =
    MyState <$> updateState x
```

Once `reducers` and `middlewares` have been attached, state watchers can be connected and commands can be sent.

Using `watch` allows for the connection of state update callbacks to view changes from reducers and middlewares.

```haskell
watch (\state -> action state)
```

Dispatching a command is performed via `command`.

```haskell
command SomeCommand
```

> The `Command` class constrains the pairing of commands with state via a functional dependency:
>
> ```haskell
> class Command state command | command -> state
> ```
>
> Thus, the type of command will determine to which state the command is dispatched. However, the type of state does not determine the type of commands, so multiple command types can be dispatched to the same state type.

Putting it all together (modulo middlewares), here is the same counter implemented with `excelsior`:

```haskell
data Counter = Counter !Int

data CountCmd = Inc | Dec
instance Command Counter CountCmd

count :: Reducer CountCmd Counter
count Inc (Counter n) = Counter (n + 1)
count Dec (Counter n) = Counter (n - 1)

counter :: View
counter = flip Component () $ \self -> def
    { construct = return 0
    , executing = \st -> do
      watch' $ \(Counter n) ->
        modify_ self $ \_ _ -> n
      pure st
    , render = \_ n ->
        Div <||>
          [ Button <| OnClick (\_ -> command Inc) |>
            [ "Increment" ]
          , text n
          , Button <| OnClick (\_ -> command Dec) |>
            [ "Decrement" ]
          ]
    }

main = inject body $
  Div <||>
    [ createStore (Counter 0) [ reducer counter ] [ ]
    , counter
    ]
```

The difference between this implementation and the component implementation or the pur-state implementation above is that the state of this counter is implicitly shared across the application; any expression that uses `command`, or calls `watch`, will witness the same counter state.

## pure-elm

[pure-elm](/doc/pure-elm) implements [The Elm Architecture](https://guide.elm-lang.org/architecture/) that encompasses a unified design ideology.

The elm architecture uses a message-based approach, like `excelsior` above, but, by default, confines the context in which the messages may be dispatched using an implicit constraint.

```haskell
type Elm msg = (?command :: msg -> IO ())
```

Unlike `excelsior`, `pure-elm` constrains the update command to a single type to maintain simplicity. That is, while `excelsior` allows for extending a store with multiple reducers that each respond to a different command type while updating the same state type, `pure-elm` has a single command type.

Unlike `excelsior`, `pure-elm` derives a `View` from the current state and updates it as the state updates.

The counter example is implemented as

```haskell
type Env = ()

type Model = Int

data Msg = Increment | Decrement

update :: Elm Msg => Msg -> Env -> Model -> IO Model
update Increment _ = pure . succ
update Decrement _ = pure . pred

view :: Elm Msg => Env -> Model -> View
view _ model =
  Div <||>
    [ Button <| OnClick (const (command Increment)) |> [ "Increment" ]
    , text model
    , Button <| OnClick (const (command Decrement)) |> [ "Decrement" ]
    ]

main = inject body (run app env)
  where
    app = App [] [] [] 0 update view

    mdl :: Model
    mdl = 0

    env :: Env
    env = ()
```

## Conclusion

As a guide, I tend to start with `pure-elm` for views, `excelsior` for global stores. I view the `pure-core` `Component` as a lower-level construct that is generally to be avoided, especially in UI code. `Component` is, clearly, more powerful - since the others are implemented with it - but that power is usually unnecessary. I reserve `pure-state` for special cases of complex, highly computational views.