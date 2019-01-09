# State management in Pure

Pure implements several approaches to state management:

* Pure components have local state that can be modified via a reference to the component.

* `excelsior` is a library that implements a redux-style global state management utility using a dynamic messaging approach for extensibility.

* `pure-state` is a library that admits monadic state management.

## `Pure`

Standard component state is a staple of Pure. All approaches to state management derive from this facility.

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
          [ Button <| OnClick (const increment) |> [ "Increment" ]
          , text n
          , Button <| OnClick (const decrement) |> [ "Decrement" ]
          ]
      }
      
main = inject body counter
```

## `excelsior`

`excelsior` uses a dynamically-typed approach to state management by allowing ADTs of commands to dictate actions to which reducers may respond. By using `watch`, components can attach listeners that react to changes in state. Dispatching a command is performed via the `command` function. The reducers (`count`, below) are pure functions, but `excelsior` permits effectful middlewares (not shown). Note that the `Command` class constrains the pairing of commands with state via a functional dependency `class Command state command | command -> state`. `excelsior` is implemented via component state management.

```haskell
data Counter = Counter !Int

data CountCmd = Inc | Dec
instance Command Counter CountCmd

count Inc (Counter n) = Counter (n + 1)
count Dec (Counter n) = Counter (n - 1)

counter = flip ComponentIO () $ \self -> def
    { construct = return 0
    , executing = void $ watch' $ \(Counter n) -> modify_ self $ \_ _ -> n
    , render = \_ n -> 
        Div <||> 
          [ Button <| OnClick (\_ -> command Inc) |> [ "Increment" ]
          , text n
          , Button <| OnClick (\_ -> command Dec) |> [ "Decrement" ]
          ]
    }
    
main = inject body $ 
  Div <||>
    [ View $ Excelsior (Counter 0) [ reducer counter ] [ ]
    , counter
    ]
```

## `pure-state`

`pure-state` uses a standard state monad approach by constraining the result of the stateful action to a type of `View`. Note that a reference to the state is required within listener handlers, so `runPureWithIO` provides the reference to the state as a function parameter. Because the views are produced monadically, this is especially useful for forms! Again, `pure-state` is implemented via component state management.

```haskell
counter = runPureWithIO (0 :: Int) $ \ref -> do
  n <- get
  pure $
    Div <||> 
      [ Button <| OnClick (\_ -> modifyRef ref succ) |> [ "Increment" ]
      , text n
      , Button <| OnClick (\_ -> modifyRef ref pred) |> [ "Decrement" ]
      ]
      
main = inject body counter
```
