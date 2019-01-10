## WebSocket APIs

[pure-websocket](/doc/pure-websocket/0.7.0.0) make API creation and use a breeze:

```haskell
newtype Name = Name String
newtype Greeting = Greeting String

mkRequest "Greet" [t|Name -> Greeting|]

api = WS.api msgs reqs
  where
    msgs = WS.none
    reqs = greet <:> WS.none
```

With access to `api`, it is possible to dispatch `Greet` requests:

```haskell
remote api ws greet (Name "Sean") $ \(Greeting g) -> print g
```

Servers can easily and safely implement apposite APIs. If the implementation doesn't match the type of the API, it will not compile.

```haskell
impl = WS.Impl api msgs reqs
  where
    msgs = WS.none 
    reqs = handleGreet <:> WS.none
    
handleGreet = handle $ do
  Greet nm <- req
  let greeting = "Hello, " ++ nm
  reply (Greeting greeting)
```

Because WebSockets are bidirectional, clients can also implement APIs.
