# The Elm Architecture

Pure implements [The Elm Architecture](https://guide.elm-lang.org/architecture/) as a library, [pure-elm](/doc/pure-elm/latest), via standard [Pure Components](/doc/pure-core/latest/Pure.Data.View/data%20View).

In contrast to the implementation of `The Elm Architecture` in `Elm`, this implementation supports:

* nesting with asynchronous execution
* property injection, a la `Components`
* command injection on property updates
* command injection on mount
* command injection on unmount

```haskell
main = inject body counter

counter :: View
counter = run app ()
  where
    app = App [] [] [] 0 update view

data Msg = Increment | Decrement

update :: Msg -> () -> Int -> IO Int
update Increment _ = pure . succ
update Decrement _ = pure . pred

view :: Elm Msg => () -> Int -> View
view _ model = 
  Div <||>
    [ Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , text model
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```

Since this `pure-elm` implementation of `The Elm Architecture` inherits both nesting and composition from `Components`, it is possible to compose applications, execute them asynchronously, and render them independently.

```haskell
-- these two counters are independent
duplication_example :: View
duplication_example =
  Div <||>
    [ counter
    , counter
    ]
```

In the above application, the environment is `()`, as in `run app ()`. A static environment like `()` will not cause changes to our application, but a more dynamic environment, like `port :: Int` or `name :: String` can effect a re-render. And if you so choose, you can react to the update in the application's `update` method by injecting a command when an environment updates.

To effect a reset of a counter from the counter's environment, you simply inject a `Reset` command on environment update.

```haskell
counter :: Int -> View
counter = run app
  where
    app = App [] [Reset] [] 0 update view

data Msg = Reset | Increment | Decrement

update :: Msg -> Int -> Int -> IO Int
update Reset     env _   = pure env
update Increment _   mdl = pure (succ mdl)
update Decrement _   mdl = pure (pred mdl)

view :: Elm Msg => Int -> Int -> View
view _ mdl =
  Div <||>
    [ Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , text model
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```

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
    -- Note the `Startup` command that is injected on application startup.
    app = App [Startup] [] [] 0 update view

data Msg = Startup | Increment | Decrement

-- Note the subscription to `Msg` messages on startup.
update Startup   _ = (subscribe $>)
update Increment _ = pure . succ
update Decrement _ = pure . pred

-- Note the use of `publish` instead of `command`, below.
view _ model = 
  Div <||>
    [ Button <| OnClick (const (publish Decrement)) |> [ "-" ]
    , text model
    , Button <| OnClick (const (publish Increment)) |> [ "+" ]
    ]
```

Now if we duplicate the counters, each live instance sees the `Increment` or `Decrement` message as if it were generated internally, and they will stay in sync. A new counter in the application, however, would not see all of the previous `Increment` and `Decrement` messages.

```haskell
duplication_example =
  Div <||>
    [ counter
    , counter
    ]
```
To learn how to manage globally accessible state, see [the state tutorial](/tut/state).
