# pure-lifted

A core library that lifts common javascript functionality for seamless integration with both frontend and backend-oriented code.

## Pure.Animation

`Pure.Animation` implements a singleton animator and exports a method to add actions to perform during the next animation frame. When compiled with GHC, the animator executes the actions synchronously.

> If `addAnimation` returns `True`, the animators queue was empty and the action can be expected to execute soon.

```haskell
addAnimation :: IO () -> IO Bool
```

## Pure.IdleWork

`Pure.IdleWork` implements an idle callback facility. Work added via `addIdleWork` will be performed during the next idle period in the browser. This is useful for performing unimportant tasks, like synchronizing non-critical data with a server, performing cleanup, etc....

In the browser, the required javascript method `requestIdleCallback` is poly-filled with a version that simply delays for a short period.

> `Pure.IdleWork` implements an idle worker for GHC that doesn't work correctly! Don't use `Pure.IdleWork` on GHC!
>
> Future versions of `Pure.IdleWork` will likely perform actions synchronously and immediately on GHC.

> If `addIdleWork` returns `True`, the workers queue was empty and the action can be expected to execute soon-ish.

```haskell
addIdleWork :: IO () -> IO Bool
```

## Pure.Data.Lifted

`Pure.Data.Lifted` is glue module the unifies frontend and backend types. For instance, `Win` is a javascript object (encapsulated by `JSV`, or javascript value) when compiled with GHCJS, but a `()` when compiled with GHC.

# TODO: Figure out what to document here. What would others need to use from this module?
