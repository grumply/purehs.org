# The Elm Architecture

[Components](/tut/components) implement a form of MVC, but require some boilerplate to pass around a reference to the stateful context. Sometimes you don't need the power of direct manipulation access to a context's state and you want a simpler interface that manages some of the boilerplate for you.

This is where [The Elm Architecture](https://guide.elm-lang.org/architecture/) comes in. Pure implements `The Elm Architecture` as a library, [pure-elm](https://github.com/grumply/pure-elm), via standard Pure [Components](/tut/components).[^1]

```haskell
main = inject body counter

counter = run app ()
  where
    app = App [] [] [] 0 update view

data Msg = Increment | Decrement

update Increment _ = pure . succ
update Decrement _ = pure . pred

view _ model = 
  Div <||>
    [ Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , text model
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```

This implementation allows for nesting and composition of controllers in an elegant way that allows them to execute asynchronously and render independently of each other, as with [Components](/tut/components).

```haskell
-- these two counters are independent
duplication_example =
  Div <||>
    [ counter
    , counter
    ]
```

Where this implementation of `The Elm Architecture` differs from the standard implementation is with the addition of the injectable environment. In the above Elm-style application, the environment is ```()``` in ```run app ()```. A static environment like ```()``` will not cause changes to our application, but a more dynamic environment, like ```port :: Int``` or ```name :: String``` can effect a re-render. And if you so choose, you can react to the update in the application's ```update``` method by creating a message for it.

```haskell
counter :: Txt -> View
counter name = run app name
  where
    app = App [] [Receive] [] 0 update view

data Msg = Receive | Increment | Decrement

update Receive env mdl = do
  doWithNewEnv env -- handle injected environment update
  pure mdl
update Increment _ mdl = pure (succ mdl)
update Decrement _ mdl = pure (pred mdl)

view env mdl =
  Div <||>
    [ fromTxt env
    , ": "
    , Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , text model
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```

Similar lifecycle hooks exist for startup and shutdown.

## Generalization

With a simple extension, `command` can be turned into a more powerful broadcast mechanism, `publish`, that is not constrained by implicit context.

Broadcast a message to any subscribed controllers.

```haskell
publish :: msg -> IO ()
```

Subscribe to any `publish`ed messages for the current implicit scope.

```haskell
subscribe :: Elm msg => IO ()
```

Or subscribe to any `publish`ed messages with a custom wrapper for injection into the current implicit scope.

```haskell
subscribeWith :: (Elm msg, wrapper ~ (a -> msg)) => wrapper -> IO ()
```

With a minor modification, the above counters can be linked together.

```haskell
main = inject body counter

counter = run app ()
  where
    app = App [Startup] [] [] 0 update view

data Msg = Startup | Increment | Decrement

update Startup   _ = (subscribe $>)
update Increment _ = pure . succ
update Decrement _ = pure . pred

view _ model = 
  Div <||>
    [ Button <| OnClick (const (publish Decrement)) |> [ "-" ]
    , text model
    , Button <| OnClick (const (publish Increment)) |> [ "+" ]
    ]
```

Now if we duplicate the counters, each live instance sees the `Increment` or `Decrement` message as if it were generated internally.

```haskell
duplication_example =
  Div <||>
    [ counter
    , counter
    ]
```

Using `The Elm Architecture` with nesting, environment injection, and a pub/sub system has been a pleasant developer experience and has tended to reduce code and dependency complexity and has increased mobility by permitting more fine-grained division of responsibility and increased compositionality.

[^1]: The implementation also relies on implicit context injection via constraint satisfaction through a hidden use of [Implicit parameters](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters) with the `Elm` contraint synonym, but this is mostly invisible to the user except for the inability to have multiple simultaneous constraints of the same type, i.e. ```(Elm Int, Elm Int)``` does not allow for two separate `Elm` contexts since it desugars to ```(?command :: Int -> IO,?command :: Int -> IO ())```. In most cases, however, the constraint can be fully omitted as GHC does a good job inferring it.
