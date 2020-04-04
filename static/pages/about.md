# What is Pure?

Pure was developed to fill the role of modern application design within the
Haskell ecosystem. Pure implements a compelling abstraction for the
implementation of web servers and services, web clients, and modern desktop
applications based on web technologies.

If you know Haskell, Elm, PureScript, or ReactJS, you'll be able to design
applications with Pure.

```haskell
counter :: IO () -> IO () -> Int -> View
counter decrement increment current =
  Div <||>
    [ Button <| OnClick (\_ -> decrement) |> [ "-" ]
    , text current
    , Button <| OnClick (\_ -> increment) |> [ "+" ]
    ]
```

Pure offers basic view primitives like HTML, SVG, and text nodes as well as
advanced primitives for portals, components, dynamically extensible views, and
keyed nodes.

By relying on GHC/JS's mutable substrate for referrential equality and 
intelligently performing as much work as possible outside of animation frames,
Pure can perform nearly all of the work of building and diffing user interfaces
using Haskell's green threads while still being responsive to user input. This
gives Pure a fair and fast rendering engine with exceptional rendering and
painting characteristics. 

By reappropriating the primitives developed for complex user interfaces, servers
and services are implemented in exactly the same fashion as client applications,
allowing for abstractions to be shared by client and server, including
server-side rendering and view serialization.

If you're interested, take a look at the [Tutorials](/tut).

If you have questions, check out the [Discourse](http://discourse.purehs.org).
