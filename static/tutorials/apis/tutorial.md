<p class="drop">
This is the sixth and final part in the [5-Minute Series](./5-minute) of Pure.hs fundamentals. In the last part, we learned how to construct servers to manage websocket connections using components and *The Elm Architecture* and we learned how to share state across disprate pieces of applications. 
</p>

In this tutorial, we'll learn how to implement a type-safe API for our backend server. Because we're continuing with server-side development, many of the code examples will not compile in the live editor, so we'll stick with simple syntax highlighting. If you want to try the code yourself, follow the [Install Tutorial](./install).

## 

```haskell
module Shared where

import Pure.WebSocket

data Status = Doom | Gloom Time
  deriving (ToJSON,FromJSON)

mkMessage "Untrigger" [t|()|]
mkRequest "GetStatus" [t|() -> Status|]

doomsday = api msgs reqs
  where
    msgs = untrigger <:> none
    reqs = getStatus <:> none
```

Let's break this down.

### Status

```haskell
data Status = Doom | Gloom Time
  deriving (ToJSON,FromJSON)
```

The status type will be not only the backend state, but also the type communicated with our API. It is required that all requests and responses can be encoded and decoded as JSON. This means some types cannot be used as requests or responses, including, importantly, functions and mutable references. However, by implementing functions in a shared module, they can be used by both frontends and backends.

### Request

The template Haskell splices

```haskell
mkMessage "Untrigger" [t|()|]
mkRequest "GetStatus" [t|() -> Status|]
```

generates instances and a proxy for using `Untrigger` and `GetStatus` as api endpoints.

```haskell
data Untrigger
instance Message Untrigger where
  type Msg Untrigger = ()

untrigger :: Proxy Untrigger
untrigger = Proxy

data GetStatus
instance Request GetStatus where
  type Req GetStatus = ()
  type Rsp GetStatus = Status

getStatus :: Proxy GetStatus
getStatus = Proxy
```

We used these generated proxies to construct the typed API.

<div class="info">
Note that the type splice given to `mkRequest` is a function type from request to response, but there is no function arrow for `untrigger`. Requests expect a response, but messages do not.
</div>

### The API

```haskell
doomsday :: API '[Untrigger] '[GetStatus]
doomsday = api msgs reqs
  where
    msgs = untrigger <:> none
    reqs = getStatus <:> none
```

The API is effectively just a way to describe the types of the endpoints. This is the interface to which we'll design the implementation of our endpoints. 

The API type is fully inferred, and the signature can be omitted. Optionally, the API can be defined as a type signature alone, and the implementation can be inferred.

```haskell
doomsday :: API '[Untrigger] '[GetStatus]
doomsday = deriveAPI
```

But again, this is a description of the types of the endpoints, not the endpoints themselves. To implement the endpoints, we'll transition back to the backend.

## Hello backend, my old friend.

Now that we have an API to work with, let's see what an implementation of the endpoints looks like. We'll extend the work we did in the last tutorial to globally manage the `Status` and write both a `getStatus` handler and an `untrigger` handler to serve our API.

```haskell
import Pure.Elm
import Pure.Server

import Shared

main = inject body (app ())

type App = Ref () Status

app = Comp $ \self -> def
  { construct = pure Doom
  , render = \_ _ -> server self
  }

server :: App -> View
server = Server "localhost" 8080 . conn

data Model = Model
data Msg = Startup

conn :: App -> WebSocket -> View
conn ref ws = run (App [Startup] [] [] (pure Model) update view) (ref,ws)
  where
    update Startup (ref,ws) mdl = do
      enact (doomsdayEndpoints ref) ws
      activate ws
      pure mdl

    view _ _ = Null

doomsdayEndpoints ref = Endpoints doomsday msgs reqs
  where
    msgs = handleUntrigger ref <:> none
    reqs = handleGetStatus ref <:> none

handleUntrigger :: App -> Awaiting Untrigger
handleUntrigger ref = awaiting $ \() -> do
  now <- time
  modify ref $ \_ _ -> do
    print "Triggering doomsday machine. The doomsday shroud will dissipate in 93 years." 
    Gloom (now + Years * 93)

handleGetStatus :: App -> Responding GetStatus
handleGetStatus ref = responding $ \() -> do
  status <- get ref
  reply status
```

As we decided in the previous tutorial, the doomsday machine only triggers if you try to prevent it from triggering.

Unlike many places in Pure.hs, endpoints are one place where type signatures (or type applications) are required because the request and message types cannot be inferred from context.

<div class="info">
Note that endpoints can be declared in any order, so long as they fully cover the API.

```haskell
myAPI = api msgs reqs
  where
    msgs = none
    reqs = reqA <:> reqB <:> none

myEndpoints = api myAPI msgs reqs
  where
    msgs = none
    reqs = handleReqB <:> handleReqA <:> none
```
</div>

Now that we have a backend server that will run our doomsday machine, we'll build a client to interact with it.

## Back to the front

```haskell
import Pure.Elm
import Pure.WebSocket

import Shared

main = do
  ws <- clientWS "localhost" 8080
  inject body (app ws)

data Model = Model (Maybe Status)
data Msg = GetStatus | SetStatus Status | Untrigger

app :: WebSocket -> View
app = run (App [] [] [] (pure (Model Nothing)) update view)
  where
    update GetStatus ws mdl = do
      remote doomsday ws getStatus (command . SetStatus)
      pure mdl

    update (SetStatus s) _ _ = pure (Model (Just s))

    update Untrigger ws mdl = do
      notify doomsday ws untrigger ()
      pure mdl

    view _ (Model ms) = 
      Div <||>
        [ Button <| OnClick (\_ -> command GetStatus) |> [ "Get Status" ]
        , Button <| OnClick (\_ -> command Untrigger) |> [ "Untrigger"  ]
        , maybe Null status ms
        ]

status :: Status -> View
status Doom = P <||> [ "Doom" ]
status (Gloom t) = P <||> [ "Gloom until ", toPrettyDate t ]
```

On `GetStatus`, our update calls `remote` with the doomsday API and its websocket with the required `()` payload. 

Once the server responds, a `SetStatus` command is sent back to the app.

On `Untrigger`, our update calls `notify` with the doomsday API and its websocket with the required `()` payload.

<div class="info">
`remote` and `notify` are asynchronous and return immediately. To make the request synchronous, a synchronizing primitive is required, like `MVar` or `TVar`.
</div>

## A Complex of Computers

Now, we didn't actually ever implement a complex of computers to manage our doomsday machine, but here's the secret: backend servers can use the same APIs to communicate with each other. We call a backend server that doesn't communicate with a client a service. So the same tools used for client and server development are used for services, as well! 

Also of import is the fact that APIs are not a server-only functionality. Websockets work both directions, and clients can implement websockets with which servers can communicate in the same way we did above.

## What have we learned?

That's it, you've learned everything. There's nothing left. If you've made it this far, you can say you know Pure.hs. Well, there's a little bit left.

We've covered a lot in this series. We learned about the core types and functions and how to use them to structure applications. We learned how to decompose applications to manage complexity by encapsulating and simplifying stateful components and pushing complexity to the leaves of view hierarchies. We learned how to use the same types and functions used for frontend development to build backend servers, and we learned how to tie those frontend and backend applications together with well-typed APIs communicating over websockets.

Really, though, it's a world of fun, and I hope you feel inspired to do something in Pure.hs and maybe join our community. 

## Next Steps

If you haven't yet, I would take a look at the [About](/about) page, as the context you have developed here will transfer to that page very well and there are a couple of bits you might pick up from the examples there.

If you want to install Pure.hs, take a look at the [Quickstart Tutorial](/tutorials/quickstart).

If you want to try Pure.hs live, as in the above examples, check out the standalone live editor and compiler at [try.purehs.org](http://try.purehs.org).

If you have questions, come to our [forum](http://discourse.purehs.org) or [chat](https://discord.gg/hVkMsEA).

<div class="prev">
[< Prev](./servers)
</div>