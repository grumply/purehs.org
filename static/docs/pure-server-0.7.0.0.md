# pure-server

A simple websocket server in both secure `wss://` and insecure `ws://` variants.

## Pure.Server

The basic idea is to mimic frontend development on the backend. To this end, the server is just a stateful component that accepts and renders active connections as keyed children. In this way, the server can seamlessly re-use libraries written to support frontend development.

It is possible to nest servers within each other for complex state-dependent service initializations. Therefore, it is best to think of Pure applications, both frontend and backend, as trees of dependent contexts enriched and extended with views.

As a default, and because the persistence enriches interaction, server-client communication is performed via [pure-websocket](/doc/pure-websocket).

### Server

The core `Server` type admits a `Pure` instance. Thus, to run a server, you must inject it via [pure-dom](/doc/pure-dom).

Here's a common server pattern that acts as a good starting point for application implementation. Note that this could be a web server or a web service, the `connection` is just an active websocket connection.

```haskell
server = Component $ \self -> def
  { construct = return mempty
  , render = \_ st -> Server host port $
      \ws -> connection (ws,st)
  , unmounted = ask self
  }

connection = Component $ \self -> def
  { construct = return mempty
  , executing = \st -> do
      (ws,st) <- ask self
      enact ws (impl self)
      WS.activate ws
      pure st
  }

main = do
  barrier <- newEmptyMVar
  let exit = putMVar barrier ()
  inject body (server exit)
  takeMVar barrier
```

`server` is initialized with with some state (as `mempty` here).

```haskell
  { construct = return mempty
```

And renders as a `Server` on a configurable `host` and `port` with each connection being rendered via `client`. Note that the client, here, is given access to the server's state.

```haskell
  , render = \_ st -> Server host port $
      \ws -> connection (ws,st)
```

> Note that the `exit` action passed to `server` prevents the main thread from exiting while the server is still working.
>
> The `unmounted` field that calls the `exit` action (via `ask self`) is never actually performed in this case, but it successfully tells GHC that as long as the exit callback isn't called, keep the main thread alive. GHC isn't quite clever enough to know that `unmounted` will never be called, and that's a good thing.

`connection`, like `server`, is initialized with some state (as `mempty` here).

```haskell
  { construct = return mempty
```

During initialization, the connection `enact`s an implementation of an API and activates the websocket (starts the receive loop in a forked thread). You may enact multiple apis on the same websocket.

> Note the sequence of enacting the API before activating the receive loop - if the receive loop is active before the API, messages may get dropped.
>
> There is not (yet) an `abolish` or `repeal` for removing an enacted API from a websocket.

```haskell
  , executing = \st -> do
      (ws,st) <- ask self
      enact ws (impl self)
      WS.activate ws
      pure st
```
