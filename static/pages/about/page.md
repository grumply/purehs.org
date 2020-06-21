# Pure.hs

Pure.hs was developed to fill the role of modern application design within the Haskell ecosystem. Pure.hs witnesses a compelling abstraction for the implementation of client applications as well as web servers and services utilizing persistent websocket connections.

If you know HTML and Haskell, Elm, PureScript, or ReactJS, you'll be able to design applications with Pure.hs.

## Simple

Pure.hs offers simple declarative view primitives like HTML, SVG, and text nodes that make it easy to get started. When more power is required, Pure.hs offers keyed nodes, components, and first-class portals.

Pure.hs also avoids complicated types, where possible, to simplify development experience and create a welcoming API for the uninitiated. Tracing down the shape of a component isn't generally a good experience for new users.

<pre data-try>
import Pure

main = inject body $
  Div <| Height (40px) . BackgroundColor gray |>
    [ P <||> [ "Hello, World!" ] 
    ]
</pre>

## Functional

Haskell's functional nature is a powerful tool for manipulation of declarative views. This page, in fact, is analyzed client-side to detect and transform the code blocks into live editors. Similarly, all links on this page are analyzed client-side and transformed to be in-app links when applicable, to avoid unnecessary page loads. All markdown code blocks are extended with click-to-copy functionality. These powerful transformations allow for expressive feature implementations with only a few lines of code.

The below example is the function used by this site to recursively analyze a tree of views for nodes containing the class `sourceCode`. If found, they're transformed into copy-to-clipboard examples.

```haskell
transform :: View -> View
transform v = _

analyze :: View -> View
analyze (Classes cs (Children vs v))
  | "sourceCode" `elem` cs = transform v
  | otherwise = SetChildren (fmap analyze vs) v
```

## Static Types

Haskell's excellent type system eases domain modeling and application design with type inference catching many common errors.

<pre data-try>
import Pure hiding (Name)

data Plebeian
data Admin

newtype Name = Name Txt

data Person auth where
  Person :: Name -> Person auth

alice :: Person Admin
alice = Person (Name "alice")

bob :: Person Plebeian
bob = Person (Name "bob")

admin :: Person Admin -> a -> a
admin _ a = a

main = inject body (admin bob "Bob can't do this, he's a pleb.")
</pre>

## Dynamic Stateful Views

Pure.hs has stateful components that allow modeling of advanced dynamic functionality, like an extended version of *The Elm Architecture* with nesting and inline effects.

<pre data-try>
import Pure.Elm

newtype Counter = Counter Int deriving (Num)

data Msg = Increment | Decrement

counter :: View
counter = run (App [] [] [] (Counter 0) update view) ()
  where
    update :: Msg -> () -> Counter -> IO Counter
    update Increment _ c = pure (c + 1)
    update Decrement _ c = pure (c - 1)

    view :: Elm Msg => () -> Counter -> View
    view _ (Counter c) = 
      Div <||>
        [ Button <| OnClick (\_ -> command Decrement) |> [ "-" ]
        , txt c
        , Button <| OnClick (\_ -> command Increment) |> [ "+" ]
        ]

main = inject body counter
</pre>

## Themeing Support

Out-of-the-bag, Pure.hs offers themeing and CSS functionality that is easy and safe.

<pre data-try>
import Pure

data GreenT
instance Theme GreenT where
  theme c = void $ 
    is c .> do
      height           =: 40px
      width            =: 40px
      background-color =: green

main = inject body $ 
  Div <| Themed @GreenT
</pre>

## JavaScript Interop

GHCJS gives Pure.hs excellent JavaScript interop as a well-integrated first-class citizen.

<pre data-try>
import Pure

foreign import javascript safe
  "alert('Clicked.')"
    alert_js :: IO ()

main = inject body $ 
  Button <| OnClick (\_ -> alert_js) |> 
    [ "Click Me!" ]
</pre>

## ReactJS-Style Components

Pure.hs uses a core stateful component type much like that of ReactJS to encapsulate complex views and expose a simple and reasoned interface to the implementation of advanced configurable abstractions. A full bevy of lifecycle methods are supported, including:

  * construction
  * pre-mount
  * thread start
  * post-mount
  * property reception 
  * update short-circuiting
  * pre-update
  * post-update, including on-commit effects per-update
  * pre-unmount

With pure-dom, updates are efficiently batched and handled in a fully multi-threaded fashion, independently of other components. Reconciliation is performed with a two-phase plan-commit approach, greatly reducing layout and render times. Component references can be passed around and manipulated together or used for cross-component and out-of-tree communication. This core component abstraction is alluring, but often much more power than required, and other approaches like *The Elm Architecture*, that is more constrained and built on components, suffices.

This component abstraction is used client-side and server-side to implement complex applications with advanced lifecycle controls and management hierarchies. The server to which this site is communicating uses a core component to manage active websocket connections, with each websocket connection being managed by a descendent, stateful component - these components can keep track of per-session information, including request history and trajectory, and can push information to the client.

<pre data-try>
import Pure

import Control.Concurrent
import Control.Monad

import System.IO

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
          , pure () -- IO action to perform after commiting update
          )
  in def
    { construct = pure (State Nothing 0)

    , mount = \st -> do
      d <- ask self
      tid <- forkIO $ forever (tick >> delay d)
      pure st { thread = Just tid }

    , unmounted = do
      st <- get self
      for_ (thread st) killThread

    , render = \_ st -> toDateTime (now st)
    }

main = do
  hSetBuffering stdout LineBuffering
  inject body (clock (1 * Second))
</pre>

## WebSockets

Pure.hs has websocket support as well as support for well-typed websocket APIs, guaranteeing that requests and responses are well-formed and APIs are fully and correctly implemented. APIs are composable to ease delineation of domains. 

Websocket communications are well-optimized, being both quick and efficient on client and server. Reconnect with exponential backoff with jitter is performed to make user experience seamless and well-managed. Throughput protections are configurable and enabled by default on servers to prevent mal-communication.

<pre data-try>
-- This module will not compile in the live editor 
-- for security reasons involving TemplateHaskell,
-- but this is an excerpt from the API for purehs.org

import Pure.WebSocket

mkRequest "ListTutorials" [t|() -> [Tutorial Markdown]|]
mkRequest "GetTutorial" [t|Slug -> Maybe (Tutorial Markdown)|]
mkRequest "GetTutorialContent" [t|Slug -> Maybe (TutorialContent Markdown)|]

tutorialAPI :: FullAPI '[] _
tutorialAPI = api msgs reqs
  where
    msgs = none
    reqs = listTutorials
       <:> getTutorial 
       <:> getTutorialContent
       <:> none
</pre>

## Threaded Runtime

By relying on GHC/JS's mutable substrate for referrential equality and intelligently performing as much work as possible outside of animation frames, Pure.hs achieves a fair and fast rendering engine that can perform nearly all of the work of building and reconciling user interfaces using Haskell's green threads while maintaining exceptional rendering and painting characteristics during animation. 

These two counters run in their own threads. When the first counter is incremented or decremented, it blocks for 5 seconds. Try incrementing the second counter while the first is blocked.

<pre data-try>
import Pure.Elm

newtype Counter = Counter Int deriving (Num)

data Msg = Increment | Decrement

counter :: Time -> View
counter = run (App [] [] [] (Counter 0) update view) 
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

## Performant

Pure.hs uses a well-optimized rendering and reconciliation algorithm and can match or, in some cases, exceed the performance of top-tier web frameworks like React, Vue, and Angular. In the general case, Pure.hs is about 50% slower than hand-optimized JavaScript.

![Some slightly outdated benchmarks, but still representative.](/static/results-small.png)

## Where to go from here?

If you're interested, take a look at the [Quickstart Tutorial](/tutorials/quickstart) to get Pure.hs or the [5-Minute Series](/tutorials/5-minute) to learn the fundamentals in easy-to-digest chunks.

If you want to try Pure.hs live, as in the above examples, check out the live editor and compiler at [try.purehs.org](http://try.purehs.org).

If you have questions, come to our [forum](http://discourse.purehs.org) or [chat](https://discord.gg/hVkMsEA). I hope you're willing to join the fun. ***Above all, [be excellent to each other](https://www.youtube.com/watch?v=rph_1DODXDU); a compassionate and helpful manner is our rule above rules!***