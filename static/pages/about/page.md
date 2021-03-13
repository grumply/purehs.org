
# A Haskell Framework

Pure.hs was developed to fill the role of modern application design within the Haskell ecosystem by implementing a compelling abstraction for client applications, web servers and services.

If you know one of Haskell, Elm, PureScript, or ReactJS, you'll be able to design applications with Pure.hs.

## Simple and Easy-to-Learn

Pure.hs offers simple declarative view primitives like HTML, SVG, and text nodes that make it easy to get started. When more power is required, Pure.hs offers keyed nodes, components, lazy views, tagged views, and first-class portals.

Pure.hs also avoids complicated types, where possible, to simplify development experience and create a welcoming API for the uninitiated. Tracing the shape and paths of a complex state managing component isn't generally a good experience for new users, so a focus has been made on maintaining simplicity and consistency everywhere possible.

<pre data-try>
import Pure

example :: View
example =
  Div <| Height (40px) . BackgroundColor gray |>
    [ P <||> [ "Hello, World!" ] 
    ]

main = inject body example
</pre>

## Functional

Haskell's functional nature is a powerful tool for manipulation of views as data. This page was served as a JSON-encoded Haskell data structure and analyzed client-side to transform code blocks into live editors. Similarly, all links on this page were analyzed client-side and transformed to be in-app links when applicable, to avoid unnecessary page loads. All non-live code blocks, like below, were extended with click-to-copy functionality. These functional transformations allow for succinct and expressive feature implementations because Pure.hs implements views as data.

The below example is the function used by this site to analyze a view for nodes containing the class `sourceCode`. If found, they're transformed into copy-to-clipboard examples. Because this is Haskell, all of the transformations are performed lazily while the page is being constructed.

```haskell
transform :: View -> View
transform v = _

analyze :: View -> View
analyze (Classes cs (Children vs v))
  | "sourceCode" `elem` cs = transform v
  | otherwise              = v <||> fmap analyze vs
```

## Static Types

Haskell's excellent type system eases domain modeling and application design with the type system catching many common errors.

<pre data-try>
import Pure 

-- Nothing about this example is particular to Pure.hs; 
-- the power of Haskell's type system is hard to overstate

data Role = User | Admin

data Person (auth :: Role) where
  Person :: { name :: Txt } -> Person auth

alice :: Person Admin
alice = Person "alice"

bob :: Person User
bob = Person "bob"

withAdmin :: Person Admin -> a -> a
withAdmin _ a = a

impossible :: View
impossible = withAdmin bob "Bob can't do this, he's not an admin."

main = inject body impossible
</pre>

## Theming Support

Pure.hs offers optional theming and CSS functionality that is easy and convenient. Most plain CSS can be copy-and-pasted with minor syntactic modification. Once you use well-typed theming with functional extensibility and type-level configuration, you'll never want to go back.
 
<pre data-try>
import Pure

data Green
instance Theme Green where
  theme c =
    is c do
      height           =: 40px
      width            =: 40px
      background-color =: green

main = inject body (Div <| Themed @Green)
</pre>

## JavaScript Interop

GHCJS gives Pure.hs excellent JavaScript interop as a well-integrated first-class citizen. The below editor is, itself, the venerable [CodeMirror](https://codemirror.net) integrated into this Pure.hs application as a well-typed and managed reference using this JavaScript interop.

<pre data-try>
import Pure

foreign import javascript safe
  "alert('You sly devil.')"
    alert_js :: IO ()

main = inject body $ 
  Button <| OnClick (\_ -> alert_js) |> 
    [ "Click Me!" ]
</pre>

## ReactJS-Style Components

Pure.hs uses a core stateful component type much like that of ReactJS to encapsulate complex views and expose a simple and reasoned interface to the implementation of dynamic state management. A [full bevy of lifecycle methods](/packages/pure-core/latest/Pure.Data.View/data%20Comp) are supported. Pure.hs unifies the power of ReactJS-style components with Haskell with a cutom reconciliation engine - these may look a bit like ReactJS components, but they're all Haskell.

<pre data-try>
import Pure

import Control.Concurrent
import Control.Monad

data State = State
  { thread :: Maybe ThreadId 
  , now    :: Time   
  }

clock :: Time -> View
clock = Component $ \self ->
  let
    tick = do
      -- update is queued and batched
      modifyM_ self $ \_ st -> do
        t <- time
        pure 
          ( st { now = t }
          , pure () -- IO action to perform after committing update
          )
  in def
    { construct = pure (State Nothing 0)

    , mount = \st -> do
      d <- ask self

      -- true multitasking without setInterval()
      tid <- forkIO do 
        forever do
          tick 
          delay d

      pure st { thread = Just tid }

    , unmounted = do
      st <- get self
      for_ (thread st) killThread

    , render = \_ st -> toDateTime (now st)
    }

main = inject body (clock (1 * Second))
</pre>

## Dynamic State Abstractions

The core Pure.hs component abstraction is alluring and powerful, but often much more powerful than required, and other approaches, like *The Elm Architecture* - while more constrained - make your components and applications easy to reason about by disentangling complex update dynamics. Pure.hs has an [implementation of The Elm Architecture](/packages/pure-elm/latest) extended with proper nesting, inline effects, a convenient publish-subscribe system for out-of-tree messaging, and an application interface for titling, routing and scroll management - all implemented top of Pure.hs components.

<pre data-try>
import Pure.Elm

import Control.Concurrent
import Control.Monad

import System.IO

data Model = Model
  { thread :: Maybe ThreadId 
  , now    :: Time   
  }

data Msg = Startup | Tick | Shutdown

clock :: Time -> View
clock = run (Applet [Startup] [] [Shutdown] init upon view)
  where init = Model Nothing 0

type Update = Elm Msg => Time -> Model -> IO Model

upon :: Msg -> Update
upon = \case
  Startup  -> startup
  Shutdown -> shutdown
  Tick     -> tick

startup :: Update
startup interval Model {..} = do
  thread <- fmap Just $ forkIO do
    forever do
      command Tick
      delay interval
  pure Model {..}

shutdown :: Update 
shutdown _ Model {..} = do
  for_ thread killThread  
  pure Model {..}

tick :: Update
tick _ Model {..} = do
  now <- time
  pure Model {..}

type Render = Elm Msg => Time -> Model -> View

view :: Render
view _ Model {..} = toDateTime now

main = inject body (clock (1 * Second))
</pre>

## A Powerful Runtime

Pure.hs runs on both GHC and GHCJS and inherits the powerful GHC runtime system. This allows Pure.hs to rely on GHC/JS's mutable substrate for referrential equality to intelligently perform as much work as possible outside of animation frames and achieve a fair and fast rendering engine that can perform nearly all of the work of building and reconciling user interfaces using Haskell's green threads while maintaining exceptional rendering and painting characteristics during animation.

These two counters run in their own threads. When the first counter is incremented or decremented, it blocks for 5 seconds. Try incrementing the second counter while the first is blocked.

<pre data-try>
import Pure.Elm

newtype Counter = Counter Int deriving (Num)

data Msg = Increment | Decrement

counter :: Time -> View
counter = run (Applet [] [] [] (Counter 0) update view) 
  where
    update Increment d c = delay d >> pure (c + 1)
    update Decrement d c = delay d >> pure (c - 1)

    view _ (Counter c) = 
      Div <||>
        [ Button <| OnClick (\_ -> command Decrement) |> [ "-" ]
        , txt c
        , Button <| OnClick (\_ -> command Increment) |> [ "+" ]
        ]

main = inject body $
  Div <||>
    [ counter (5 * Second)
    , counter 0
    ]
</pre>

## Efficient Reconciliation

Pure.hs uses a well-optimized rendering and reconciliation algorithm and can match or, in some cases, exceed the performance of top-tier web frameworks like React, Vue, and Angular.

With [pure-dom](/packages/pure-dom/latest), updates are efficiently batched and handled in a fully multi-tasked fashion, independently of other components. Reconciliation is performed with a two-phase plan-commit approach, with exceptional layout and render characteristics. This can reduce many updates to single calls like `replaceChild()`, `appendChild()`, or `classList.add()`, allowing for Pure.hs to use animation frames for updates without worries about layout blocking and long-running animation frames while keeping an expressive high-level reconciliation algorithm. Even cleanup can often be deferred to idle callbacks.

In the general case, Pure.hs is about 50% slower than hand-optimized JavaScript.

![Some slightly outdated benchmarks, but still representative.](/static/results-small.png)

Everything within this website, except for the hero at the top of the home page, is served dynamically, as JSON. No extraordinary optimizations are enacted to make it especially performant, but you should find every page to load efficiently (except for some firefox bugs with blurring). But, while the performance is a great reason to choose Pure.hs, the real power of Pure.hs is the integration of Haskell with familiar development approaches.

## WebSockets

Pure.hs has built-in websocket support as well as support for well-typed websocket APIs that guarantee requests and responses are well-formed and APIs are fully, and type-correctly, implemented. APIs are composable and concatenateable to ease delineation of domains. 

Websocket communications are well-optimized, being both quick and efficient on client and server. Reconnect with exponential backoff with jitter is performed to make user experience seamless and well-managed. Throughput protections are configurable and enabled by default on servers to prevent mal-communication. 

While websockets are available, they're not required - you may use whatever backend and communication approach you deem necessary or convenient.

<pre data-try>
import Pure.WebSocket

-- This module will not compile in the live editor 
-- for security reasons involving TemplateHaskell,
-- but this is an excerpt from the API for purehs.org

mkRequest "ListTutorials" 
  [t|() -> [Tutorial Markdown]|]

mkRequest "GetTutorial" 
  [t|Slug -> Maybe (Tutorial Markdown)|]

mkRequest "GetTutorialContent" 
  [t|Slug -> Maybe (TutorialContent Markdown)|]

tutorialAPI :: API '[] _
tutorialAPI = api msgs reqs
  where
    msgs = none
    reqs = listTutorials
       <:> getTutorial 
       <:> getTutorialContent
       <:> none
</pre>

## Client and Server

The Pure.hs component abstraction can be used server-side to implement complex applications with advanced lifecycle controls and management hierarchies. The server to which this site is communicating uses a core component to manage active websocket connections, with each websocket connection being managed by a descendent, stateful component - these components can keep track of session information and can push information to the client.

This is why the Pure.hs focuses on the concept of dynamic hierarchical contexts rather than web components. The hierarchical context abstraction is larger than client-side web development and the unification permits learning the abstractions once for frontend and implicitly having it available for backend, including servers and services, if you choose to use Pure.hs in a full-stack fashion.

```haskell
import Pure.Server
import Pure.Elm

data Msg = Startup | Shutdown

data Model = Model

conn :: WebSocket -> View
conn = run (Applet [] [] [] Model upon view)

type Update = Elm Msg => WebSocket -> Model -> IO Model

upon :: Msg -> Update
upon = \case
  Startup  -> startup
  Shutdown -> shutdown

startup :: Update
startup = _

shutdown :: Update
shutdown = _

view _ _ = Null

main = inject body (Server "localhost" 8080 conn)
```

## Where to go from here?

If you're interested, take a look at the [Quickstart Tutorial](/tutorials/quickstart) to get Pure.hs or the [5-Minute Series](/tutorials/5-minute) to learn the fundamentals in easy-to-digest chunks.

If you want to try Pure.hs live, as in the above examples, check out the live editor and compiler at [try.purehs.org](http://try.purehs.org).

If you have questions, come to our [forum](http://discourse.purehs.org) or [chat](https://discord.gg/hVkMsEA). I hope you're willing to join the fun. ***Above all, [be compassionate and helpful](https://www.youtube.com/watch?v=rph_1DODXDU)***