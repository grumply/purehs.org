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

Pure leverages the power of Haskell's lazy evaluation to implement a fair and
fast rendering engine. By relying on GHC/JS's mutable substrate for referrential
equality and intelligently performing as much work as possible outside of
animation frames, Pure can perform nearly all of the work of building and
diffing user interfaces using Haskell's green threads while still being
responsive to user input, for a scalable user experience.

By reappropriating the primitives developed for complex user interfaces, servers
and services are implemented in exactly the same fashion as client applications,
allowing for abstractions to be shared by client and server.

If you're interested, take a look at the [Tutorials](/tuts).

If you have questions, check out the [Discourse](http://discourse.purehs.org).
