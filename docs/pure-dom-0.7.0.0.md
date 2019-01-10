# pure-dom

This package implements core functionality for view injection and reconciliation. 

You probably don't need to include this package as a dependency of your project, as it is exported from the meta-package [pure](/doc/pure/0.7.0.0).

## Pure.DOM

Core injection functionality underpinning all Pure applications.

### `inject`

```haskell
inject :: IsNode n => n -> View -> IO ()
```

Inject renders the given view, recursively, and injects the result into the given DOM node as the last child. This function does not know or care about the liveness of the supplied node.

You will use `inject` in every Pure application.

```haskell
-- frontend
main = inject body "Hello, World!"
```

With GHC (optionally on GHCJS), you will have to control when the application exits via an `MVar`, or similar. This is the common pattern:

```haskell
-- backend
main = do
  barrier <- newEmptyMVar
  let exit = putMVar barrier ()

  -- Server should call the `exit` callback to notify 
  -- the main thread that it is safe to exit.
  inject body (server exit)

  -- When barrier is filled, main will exit.
  takeMVar barrier
  

server exit = {...}
```

See [pure-server](/doc/pure-server/0.7.0.0) for information of server-side Pure.

