# pure-dom

This package implements core functionality for view injection and reconciliation.

You probably don't need to include this package as a dependency of your project, as it is exported from the meta-packages [pure](/docs/pure) and [pure-elm](/docs/pure-elm).

To construct a web server, see [pure-server](/docs/pure-server).

## Pure.DOM

`inject` runs a `View` in a given context.

### `inject`

```haskell
inject :: IsNode n => n -> View -> IO ()
```

In the browser, `inject` constructs the DOM nodes for the given `View`, recursively, and injects the result into the given DOM node as the last child.

You will use `inject` in every Pure application.

```haskell
main = inject body "Hello, World!"
```

See [pure-server](/docs/pure-server) for running a websocket-based server.

