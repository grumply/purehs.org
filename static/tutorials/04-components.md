# Components

Without state management, static `View`s can only go so far. [Components](/doc/pure-core/latest/Pure.Data.View/data%20Comp) enrich a `View` with state management where changes to state effect changes in the rendered `View`. The resolution of this dynamic state to view propagation is called reconciliation.

Consider a simple counter that takes two actions:

* A decrement action to be effected when a `-` button is clicked
* An increment action to be effected when a `+` button is clicked

The counter will also receive a current value that is to be modified by each of the actions.

```haskell
counterView :: IO () -> IO () -> Int -> View
counterView decrement increment current =
  Div <||>
    [ Button <| OnClick (const decrement) |> [ "-" ]
    , text current
    , Button <| OnClick (const increment) |> [ "+" ]
    ]
```

Without state management and reconciliation, the counter will always show `0`.

To manage changes in the `counterView`, the counter state is localized to a `Component`. 

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

`update` is the method by which updates to the counter are effected, allowing the transformation of the `Int` state. The initialization of the `Component`, `construct`, pulls the initial state from its environment with `ask`.

This component can be injected as a stand-alone application.

```haskell
main = inject body (counter 0)
```

`Component`s are nestable and are designed to execute asynchronously. Thus, each `Component` is a break in the dynamic hierarchy, allowing for independent reconciliation and rendering.

Like [ReactJS Components](https://reactjs.org/docs/components-and-props.html), `Pure Components` implement a form of MVC, but require some boilerplate to pass around either a reference to the stateful context (`self`, above), or methods partially applied to a stateful context (`update`, above). A refined message-based interface with an implicit `self` can often simplify applications. This is where [The Elm Architecture](https://guide.elm-lang.org/architecture/) shines and should be a preferred default choice for most users. [pure-elm](/doc/pure-elm/latest) implements this abstraction on top of `Component` which means that `Component` is strictly more powerful. That power, however, is generally unncessary, and tends to make applications more verbose. See the [pure-elm tutorial](/tut/elm) to learn how to write applications in the style of `The Elm Architecture`.

See the [server tutorial](/tut/servers) to learn how the same `Component` abstraction is used to write generic hierarchical computational contexts, including servers and services.
