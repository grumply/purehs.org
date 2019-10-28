# pure-elm

This package implements [The Elm Architecture](https://guide.elm-lang.org/architecture/).

There is a tutorial at [purehs.org/tut/Elm](http://purehs.org/tut/Elm) describing the Elm architecture and rationale for its use.

An implementation of the button example from [elm-architecture-tutorial](https://github.com/evancz/elm-architecture-tutorial/blob/master/examples/01-button.elm).

```haskell
module Main where

import Pure.Elm

main = inject body (run app env)
  where
    app = App [] [] [] 0 update view

    mdl :: Model
    mdl = 0

    env :: Env
    env = ()

-- Types

type Env = ()

type Model = Int

-- Update

data Msg = Increment | Decrement

update :: Elm Msg => Msg -> Env -> Model -> IO Model
update Increment _ = pure . succ
update Decrement _ = pure . pred

-- View

view :: Elm Msg => Env -> Model -> View
view _ model =
  Div <||>
    [ Button <| OnClick (const (command Decrement)) |> [ "-" ]
    , Br
    , text model
    , Br
    , Button <| OnClick (const (command Increment)) |> [ "+" ]
    ]
```

## Pure.Data.Elm

`Pure.Data.Elm` exports the core functionality required to implement pure applications with the message-oriented approach of the Elm architecture. It also re-exports [pure](/doc/pure).

### data App

The core application type, `App`, defines all of the associated functionality of an Elm-style application.

```haskell
data App env st msg = App
  { _startup  :: [msg]
  , _receive  :: [msg]
  , _shutdown :: [msg]
  , _model    :: st
  , _update   :: Elm msg => msg -> env -> st -> IO st
  , _view     :: Elm msg => env -> st -> View
  }
```

`App` is usually constructed in-line rather than as a record.

```haskell
myApp mdl = App [] [] [] mdl update view

update msg env mdl = _

view env mdl = _
```

### type Elm

`Elm` is a constraint of an implicit `?command` function.

```haskell
type Elm msg = (?command :: msg -> IO Bool)
```

Given

```haskell
f :: Elm Msg => IO ()
```

`f` has access to `?command :: Msg -> IO ()` that permits the use of `command`.

### run

`run` takes an `App` and an environment and turns it into a `View`. If the call to `run` has been added to a dynamic view, changes to the environment will be injected into the app. These changes can be intercepted via the `App._receive` messages. The view will be updated with the new environment. While the environment is dynamic, the `App` is static.

> Note: the style of dynamicity that would be achieved with a dynamically updated `App` type is limited to, and recoverable in, the `App._update` and `App._view` fields.

```haskell
run :: App env st msg -> env -> View
```

### command

`command` uses the implicit supplied by the `Elm` constraint to inject a message into the enclosing application - the `App` to which the `Elm` constraint corresponds.

```haskell
command :: Elm msg => msg -> IO ()
command = void . ?command
```

### map

`map` over an `Elm` constraint to produce a newly constrainted value. Note that the constrainted value is polymorphic in its type; it could be a `View` or a `_ -> _ -> _`

```haskell
map :: (msg -> msg') -> (Elm msg => a) -> (Elm msg' => a)
```

### subscribe

`subscribe` is an extension to the message-oriented nature of `App` to permit reception of messages from a global message broker.

```haskell
subscribe :: Elm msg => IO ()
```

To subscribe, the `Elm msg` context must be satisifed. This is always the case in an `App._update`. Thus, the common way to initiate a `subscribe`, which will persist until the `App` has been removed from the live view is to use an initialization message.

```haskell
myApp = App [Startup] [] [] mdl update ...

update :: Elm Msg => Msg -> Env -> Model -> IO Model
update Startup _ mdl = do
  subscribe -- resolves to (Elm Msg) => IO ()
  pure mdl
```

Instead of receiving `command` messages, the `subscribe` receives `publish` messages.

### subscribeWith

`subscribeWith` extends `subscribe` with a lifting function.

```haskell
subscribeWith :: Elm msg => (msg' -> msg) -> IO ()
```

To subscribe to messages of type `msg'`, the `Elm msg` constraint must be satisfied and a lifting function `msg' -> msg` must be supplied.

### subscribe'

In cases where the subscription is not expected to be permanent, `subscribe'` may be used.

```haskell
subscribe' :: Elm msg => IO Unique
```

### publish

`publish` broadcasts a message to all subscribed contexts.

```haskell
publish :: msg -> IO ()
```

### publishing

`publishing` transforms a `Elm msg => a` constrainted value into an unconstrainted value that publishes messages instead of using a supplied `Elm msg` constraint.

```haskell
publishing :: (Elm msg => a) -> a
```

### unsubscribe

`unsubscribe` removes a subscription from the global broadcast broker.

```haskell
unsubscribe :: Elm msg => Unique -> IO ()
```

> This method requires TypeApplications.
>
> ```haskell
> unsubscribe @SomeMsgType myUniqueSubscriptionId
> ```

### unsubscribeWith

`unsubscribeWith` is equivalent to `unsubscribe`, but does not require `TypeApplications` - instead preferring `Proxy` type.

```haskell
unsubscribeWith :: Proxy msg -> Unique -> IO ()
```
