# About

Pure is designed for concise, expressive, and performant Haskell web servers,
services, and clients. A fully capable web framework with an extensive 
ecosystem of auxiliary libraries and tools. With Haskell's propensity for 
abstraction and safety, Pure is able to utilize all of the powerful features
expected of Haskell development.

## Clients

On the client side, Pure leverages the power of Haskell's lazy evaluation to 
implement a fair and fast rendering engine. By intelligently performing as 
little work as possible within rendering frames, Pure can fairly interleave
nearly all of the work of building and diffing user interfaces, for a truly
scalable experience.

By digging into the substrate of the GHC/JS runtime system, Pure leverages the
power of referential equality to deduplicate work and improve performance in
common cases, resulting in asymptotic improvements when updating UI elements.

Pure gets out of your way by offering a layer of reusable view types, both
declarative view primitives and react-style components, over which abstractions 
can be freely layered to achieve your desired development flow. Components 
enable thread fencing and low-level control of lifecycle methods when you really
need to dig in and control DOM primitives, and declarative view primitives 
enable exceptional abstraction and compositional programming techniques. 

With Pure, you can mix and match template-style programming, the elm
architecture, react-style components, property-oriented reusable components, 
monad transformers, redux-style state management, or create a new abstraction
that works for you and your developers.

With Pure's exceptional capability to abstract, there exist libraries that allow
you to never write another .html, .css, or .js file ever again; everything can
be done in Haskell. Pure offers a complete domain specific language for CSS+3
with even more expressiveness than even CSS3 offers - use functional programming
techniques to implement CSS templating functions, variable capture, and even
block-level reuse. Implement your CSS in such a way to programmatically generate 
complex abstractions over concepts like colors, margins, padding, layering, all
without a custom CSS templating compiler step and even at runtime with minimal
overhead.

Pure has been painstakingly constructed to maintain a minimal footprint so you
don't have to worry about JS bloat. Using advanced deduplication techniques
implemented in GHCJS and then running Google's closure compiler plus GZip,
this website transfers, over-the-wire, less than 250KB. This minimal cost is
dwarfed by the extraordinary abstraction capabilities and development benefits
of Pure.

## Servers

On the server side, Pure is generic in its use-cases and takes advantage of the
abstractions created for client-side development. 

By reappropriating the primitives developed for complex user interfaces, Pure 
servers and services are implemented in exactly the same fashion as client 
applications. 

Both servers and services, based on network availability, can run in 
client-facing or internal configurations, allowing for complex topologies to be
implemented with ease. 

## Learn Pure

* [Basics](/tuts/basics)
* [Components](/tuts/components)
* [CSS & Themeing](/tuts/css)
* [Routing](/tuts/routing)
* [WebSockets](/tuts/websockets)
* [Servers](/tuts/servers)
* [State](/tuts/state)
