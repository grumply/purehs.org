# State management in Pure

Pure implements several approaches to state management:

* [1](#Pure) [Pure components](/doc/pure-core/0.7.0.0) have internal state that can be modified locally and non-locally via a reference to the component.

* [2](#pure-state) [pure-state](/doc/pure-state/0.7.0.0) implements monadic state management.

* [3](#excelsior) [excelsior](/doc/excelsior/0.7.0.0) implements redux-style global state management using dynamic messaging à la Marlow's [An extensible dynamically-typed hierarchy of exceptions](https://doi.org/10.1145/1159842.1159854).

## `Pure`

Standard component state is a staple of Pure. All approaches to state management derive from this facility, including [pure-state](/doc/pure-state/0.7.0.0) and [excelsior](/doc/excelsior/0.7.0.0).

We initialize the state within the `construct` field and we write component-local (let-bound) update functions. Note that these let-bound update functions may be freely passed throught your application and return `True` if the component is still alive.

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

## `pure-state`

`pure-state` is a stateful view builder. Because the views can be produced monadically, this is especially useful for forms!

`pure-state` is implemented via component state management.

```haskell
counter = runPureWithIO (0 :: Int) $ \ref -> do
  n <- get
 
  let upd f _ = modifyRef ref f
  
  pure $
    Div <||> 
      [ Button <| OnClick (upd succ) |> 
        [ "Increment" ]
      , text n
      , Button <| OnClick (upd pred) |> 
        [ "Decrement" ]
      ]
      
main = inject body counter
```

> Note that a reference to the state is required within listener handlers (`OnClick`, above), so `runPureWithIO` provides the reference to the state as a function parameter. 

## `excelsior`

`excelsior` uses a dynamically-typed approach to state management by allowing ADTs of commands to dictate actions to which reducers and reducer transformers may respond. 

`excelsior` is implemented via component state management.

### `Reducer`

Reducers are pure functions that transform state:

```haskell
type Reducer state command = command -> state -> state

myReducer :: Reducer MyState MyCommand
myReducer SomeCommand (MyState x) = MyState (someStateModifier x)
```

### `Middleware`

Middlewares are reducer transformers:

```haskell
type Middleware state command = (command -> state -> IO state) -> (command -> state -> IO state)

myMiddleware :: Middleware MyState MyCommand
myMiddleware continue SomeCommand (MyState x)
  | someConditional x = do
    x' <- effectfullyUpdateState x 
    continue SomeOtherCommand (MyState x')
  | otherwise = do
    x' <- effectfullyCreateNewState
    return (MyState x')
```

> Note that, above, in the `otherwise` case, the return of `MyState x'` stops any other `Middleware`s or `Reducer`s from running.

### `watch`

By using `watch`, components can attach listeners that react to changes in state:

```haskell
watch (\state -> action state)
```

> Whereas `watch` will wait for updates before executing for the first time, `watch'` will execute immediately with the current state.

### `command`

Dispatching a command is performed via the `command` function:

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

### Demonstration

Putting it all together (modulo middlewares), here is the same counter implemented with `excelsior`:

```haskell
data Counter = Counter !Int

data CountCmd = Inc | Dec
instance Command Counter CountCmd

count Inc (Counter n) = Counter (n + 1)
count Dec (Counter n) = Counter (n - 1)

counter = flip ComponentIO () $ \self -> def
    { construct = return 0
    , executing = void $ 
      watch' $ \(Counter n) -> 
        modify_ self $ \_ _ -> n
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

The difference between this implementation and the component implementation above is that the state of this counter is implicitly shared across the application; any expression that uses `command`, or calls `watch`, will witness the same counter state.
