## Components

What about dynamic views that react to user interaction?

`counterView` constructs a dynamic view:

```haskell
counterView :: ((Int -> Int) -> IO ()) -- ^ A function that updates the counter
                                       --   with any supplied transformation.
            -> Int                     -- ^ Current value of the counter.
            -> View
counterView updater current =
  Div <| Class "current" |>
    [ decrement
    , text current
    , increment
    ]
  where
    button :: (Int -> Int) -> View -> View
    button f txt = 
      Button <| Class "action" . OnClick (\_ -> updater f) |>
        [ txt ]

    decrement = button pred "-1" 
    increment = button succ "+1"
```

Every click of the `increment` button will call the supplied updater with 
`\n -> n + 1`. `decrement` will call the supplied update function with 
`\n -> n - 1`.

To make the state mutable, a delineation must be made between immutable views
and mutable views. In `Pure`, this delineation is a react-style component that
has access to a method of self-mutation.

`counter` takes a starting value and constructs a self-contained component:

```haskell
counter :: Int -> View
counter = Component $ \self ->
  let 
    update :: (Int -> Int) -> IO ()
    update f = modify_ self $ \_ -> f
  in 
    def 
      { construct = ask -- use the supplied `Int` to start
      , render = \_ current -> counterView update current
      }
```

This component can be injected as a stand-alone application:


```haskell
main = inject body (counter 0)
```

After the increment or decrement buttons are clicked, the component will respond
to changes in state by updating the DOM. This is called reconciliation. In this 
case, the only required reconciliation is with the displayed value inside
`counterView` - a quick diff. 

Pure components, like react components, have lifecycle methods built in that
respond at different points in the life of the component:

```haskell
data Comp props state = Comp
  { initialize   :: state -> IO state
  , initialized  :: IO ()
  , construct    :: IO state
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

`initialize`, `initialized`, `construct`, `mount`, `executing`, `mounted`, and
`unmounted` only ever execute once. `receive`, `force`, `update`, `render`, and
`updated` run as often as the supplied component properties or the component 
state changes.

The above `counter` could pass the self reference `self :: Ref Int Int` to 
nested views, allowing for remote transformation of the state via 
`modify_ :: Ref p s -> (p -> s -> s) -> IO ()`. 


