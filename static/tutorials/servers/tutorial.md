<p class="drop">
This is the fifth part in the [5-Minute Series](./5-minute) of Pure.hs fundamentals. In the last part, we learned how to decompose applications to manage complexity by encapsulating and minimizing state management domains. 
</p>

In this tutorial, we'll learn how to implement a backend server using the same abstractions we learned previously in this series. Because we're switching to server-side development, many of the code examples will not compile in the live editor, so we'll stick with simple syntax highlighting. If you want to try the code yourself, follow the [Install Tutorial](./install).

## The Doomsday Gap

Alright, we're not heathens. A doomsday machine controlled by man is not a sane weapon. Such a weapon must be designed to trigger itself automatically. Any attempt to un-trigger it must, in fact, trigger it. It is not only possible to build such a machine, it is essential; we must rule out human meddling and make the machine fully automated to instill terror in our enemies. The technology required is easily within our means; we require only the will to build it. 

Such a machine is remarkably simple to build: we must construct a gigantic complex of computers that look for a specific and clearly defined set of circumstances under which the machine is to be triggered.

Firstly, we need to know how to construct a backend server that can communicate with other computers within the complex.

## A Gigantic Complex of Computers

To build backend servers, we'll use the same `View` type we used for frontend development. While we're not interested in HTML, SVG, or text nodes, we are interested in the hierarchical nature of views and the lifecycle mangement of components, specifically.

```haskell
import Pure.Server

main :: IO ()
main = inject body server

server :: View
server = Server "localhost" 8080 conn

conn :: WebSocket -> View
conn _ = Null
```

It's that easy. The server will listen for websocket connections on `localhost:8080` and run `conn` for each valid websocket connection. Now, we can focus on the implementation of `conn` using a stateful component to manage the connected websocket. 

<div class="info">
`body`, when compiled for the backend, is just a newtype around `()` rather than a newtype around a DOM node. 
</div>

Our server is now, in familiar HTML markup notation, equivalent to this:

```html
<body>
  <server>
    <conn/>
    <conn/>
    <conn/>
  </server>
</body>
```

The server will manage active connections and remove them from the `View` tree when a disconnect occurs. 

## Everything Old Is New Again

If we wanted to have shared state available to all connections, we could implement a parent component above the server that passes it's reference down through the server and to the connections.

```haskell
import System.Environment
import Pure.Server

main :: IO ()
main = do
  as <- getArgs
  inject body (app as)

data State = State

app :: [String] -> View
app = Comp $ \self -> def
  { construct = pure State
  , render = \_ _ -> server self
  }

server :: Ref [String] State -> View
server = Server "localhost" 8080 . conn

conn :: Ref [String] State -> WebSocket -> View
conn appRef ws = Null
```

This is why the hierarchical component structure is so powerful: we've used a simple component to extend the application with access to shared state with only a few lines of code. All of the independent threaded connections can interact with a shared application state.

For the connected websockets, we'll require a state-managing `View`, the familiar `Component`. On startup, we'll need to enact our doomsday API and then activate our websocket.

```haskell
data State = State

conn :: WebSocket -> View
conn = Comp $ \self ->
  def
    { construct = do
      ws <- ask self
      enact doomsday ws
      activate ws
      pure State
    , unmounted = do
      -- when the connection is closed
      pure ()
    }
```

If we were to activate the websocket before enacting the doomsday API, we could miss valid messages. This split between `enact` and `activate` allows us to enact multiple APIs before processing messages.

If we wanted to give the enacted API access to [get](/packages/pure-core/latest/Pure.Data.View/get), [ask](/packages/pure-core/latest/Pure.Data.View/ask), and [modifyM](/packages/pure-core/latest/Pure.Data.View/modifyM), we could pass `self` to it, as we did with the shared application state example, above.

```haskell
      enact (doomsday self) ws
```

While this component approach is very general and powerful, I prefer to start with *The Elm Architecture*, as the focus on state-transitions makes it easy to reason about application states. I tend to reach for the lower-level component approach only when I need the power of the advanced lifecycle methods.

```haskell
data Model = Model

data Msg = Startup

conn :: WebSocket -> View
conn = run (App [Startup] [] [] (pure Model) update view)

update :: Elm Msg => Msg -> WebSocket -> Model -> IO Model
update Startup ws mdl = do
  enact doomsday ws
  activate ws
  pure mdl

view _ _ = Null
```

In this approach, since our `Elm Msg` constraint enables the ability to send messages to the connection, nothing needs to be passed to `doomsday`, it inherits the constraint satisfaction and, thus, can send messages to `conn`. 

## A Nest of a Problem

If we switch to only using *The Elm Architecture* for our application state and our connections, we run into a problem that we haven't come across yet. It's an important issue, so let's take a minute to look at the problem in action and figure out what we can do about resolving it before we move on.

If we compile this example, we'll get a compile-time error about constraint satisfaction.

<pre data-try>
import Pure.Elm
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body a

data ModelA = ModelA
data MsgA = MsgA

a :: View
a = run (App [] [] [] (pure ModelA) update view) ()
  where
    update MsgA _ mdl = print "Got MsgA in (a)" >> pure mdl
    view _ _ = b

data ModelB = ModelB
data MsgB = MsgB

b :: Elm MsgA => View
b = run (App [] [] [] (pure ModelB) update view) ()
  where
    update MsgB _ mdl = print "Got MsgB in (b)" >> pure mdl
    view _ _ = 
      Div <||>
        [ Button <| OnClick (\_ -> command MsgA) |> [ "Send a message to A" ]
        , Button <| OnClick (\_ -> command MsgB) |> [ "Send a message to B" ]
        ]
</pre>

The type we want for `b.view` is 

```haskell
view :: (Elm MsgA, Elm MsgB) => () -> ModelB -> View
```

but the type it has is 

```haskell
view :: Elm MsgB => () -> ModelB -> View
```

because local `Elm` constraints overwrite inherited constraints.

In a nutshell, only one `Elm` constraint can be satisfied per context. Thus, if we're nesting components using *The Elm Architecture*, we can't communicate with both of them. Luckily, there are two approaches to resolving this dilemma.

### Reification

The first solution is to reify the messaging channel and pass around the functional reification in the same way we do with components and the `self` reference.

<pre data-try>
import Pure.Elm
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body a

data ModelA = ModelA
data MsgA = MsgA

a :: View
a = run (App [] [] [] (pure ModelA) update view) ()
  where
    update MsgA _ mdl = print "Got MsgA in (a)" >> pure mdl
    view _ _ = b command

data ModelB = ModelB
data MsgB = MsgB

b :: (MsgA -> IO ()) -> View
b = run (App [] [] [] (pure ModelB) update view)
  where
    update MsgB _ mdl = print "Got MsgB in (b)" >> pure mdl
    view channelA _ = 
      Div <||>
        [ Button <| OnClick (\_ -> channelA MsgA) |> [ "Send a message to A" ]
        , Button <| OnClick (\_ -> command  MsgB) |> [ "Send a message to B" ]
        ]
</pre>

Note that the reification can be constructed anywhere the constraint is satisifed. It is valid to construct the reification even within `b`, so long as it is performed within the context of `Elm MsgA`.

The following would be a valid reification.

```haskell
b :: Elm MsgA => View
b = run (App [] [] [] (pure ModelB) update view) ()
  where
    sendA :: MsgA -> IO ()
    sendA = command

    update MsgB _ mdl = print "Got MsgB in (b)" >> pure mdl
    view _ _ = 
      Div <||>
        [ Button <| OnClick (\_ -> sendA   MsgA) |> [ "Send a message to A" ]
        , Button <| OnClick (\_ -> command MsgB) |> [ "Send a message to B" ]
        ]
```

<div class="info">
The type signature for `sendA` is required in this approach, otherwise it will be inferred as

```haskell
sendA :: Elm MsgA => MsgA -> IO ()
```

and we'll be back to the same problem.
</div>

### Subscribe to Our Publication

The second solution is an extension to *The Elm Architecture* that Pure.hs implements. It is a global pub/sub system that requires explicit use, but the end result is that the `Elm` constraints are no longer required. It also freely works along-side the standard `Elm` constraint approach and permits cross-tree or out-of-line messaging without reference passing.

<pre data-try>
import Pure.Elm
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body a

data ModelA = ModelA
data MsgA = StartupA | MsgA

a :: View
a = run (App [StartupA] [] [] (pure ModelA) update view) ()
  where
    update StartupA _ mdl = subscribe >> pure mdl
    update MsgA _ mdl = do
      print "Got MsgA in (a)" 
      publish MsgB
      pure mdl
    view _ _ = b

data ModelB = ModelB
data MsgB = StartupB | MsgB

b :: View
b = run (App [StartupB] [] [] (pure ModelB) update view) ()
  where
    update StartupB _ mdl = subscribe >> pure mdl
    update MsgB _ mdl = print "Got MsgB in (b)" >> pure mdl
    view channelA _ = 
      Div <||>
        [ Button <| OnClick (\_ -> publish MsgA) |> [ "Send a message to A which will send a message to B" ]
        , Button <| OnClick (\_ -> command MsgB) |> [ "Send a message to B" ]
        ]
</pre>

Instead of using `command MsgA`, we use `publish MsgA` to dispatch the message to interested components. Any components that have already subscribed to that type of message will receive the publication. In this case, `a` has subscribed to `MsgA` messages and `b` has subscribed to `MsgB` messages.

Under the hood, this approach uses a message broker to manage the subscribers and dispatch of publications. This is a useful tool when state management begins to turn into spaghetti.

## What have we learned?

We've learned that servers are implemented in much the same way as client applications, even using the same abstractions, types, and design approaches. We learned that nested Elm-style components can get a little hairy, but there are solutions that exist that recover a reasonable experience and even a powerful abstraction, publish/subscribe, that works well with Elm-style components.

We've alluded to the API, even mentioned it in the examples, but we haven't defined it yet. In the final tutorial in this series, [Part 6: APIs](./apis), we'll learn about well-typed websocket APIs and how to use and integrate them with our stateful application and connections to finish our doomsday machine.

<div class="prev">
[< Prev](./composition)
</div>

<div class="next">
[Next >](./apis)
</div>