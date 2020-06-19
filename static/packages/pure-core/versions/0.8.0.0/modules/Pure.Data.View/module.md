## data Target

A type used to tag event listeners with their binding destination. Listeners may be bound to the local target element, the global window, or the global document. This type is generally not used, instead relying on the patterns developed in [pure-events](/packages/pure-events/latest) to instantiate targeted listeners.

```haskell
data Target
  = ElementTarget
  | WindowTarget
  | DocumentTarget
  deriving Eq
```

<div class="hide">
<pre data-try>
import Pure hiding (button)
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body button

button =
  -- OnDoc uses `DocumentTarget` under the hood
  -- which tells the application to attach the listener to 
  -- the document. When this button is removed from the page,
  -- the document listener is disassociated from the document.
  Button <| OnDoc "click" (\_ -> print "Clicked") |> 
    [ "Click" ]
</pre>
</div>


## data Listener

A type representing event listeners to be applied to [View](Pure.Data.View/data%20View). This type is generally not used, instead relying on the patterns developed in [pure-events](/packages/pure-events/latest) to instantiate listeners.

```haskell
data Listener =
  On
    { eventName     :: Txt
    , eventTarget   :: Target
    , eventOptions  :: Options
    , eventAction   :: Evt -> IO ()
    , eventStopper  :: IO ()
    }
```

<div class="hide">
<div class="info">
[Options](/packages/pure-lifted/0.8.0.0/Pure.Lifted/data%20Options) and [Evt](/packages/pure-lifted/0.8.0.0/Pure.Lifted/data%20Evt) are defined in [pure-lifted](/packages/pure-lifted/latest).
</div>

<pre data-try>
import Pure hiding (button)
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body button

button =
         -- OnClick uses (On :: Listener) under the hood
  Button <| OnClick (\_ -> print "Clicked") |> 
    [ "Click" ]
</pre>
</div>

## data Lifecycle

A type representing low-level access to context during, or immediately following, [View](Pure.Data.View/data%20View) construction. This type is often necessary for constructing complex animated user interfaces.

```haskell
data Lifecycle =
  HostRef
    { withHost :: Node -> IO () }
```

<div class="hide">
<pre data-try>
import Pure

main = inject body (simple ())

simple = Component $ \self ->
  let setHost n = modify_ self $ \_ _ -> Just n
  in def 
    { construct = pure Nothing
    , render = \_ _ -> Div <| WithHost setHost
    }
</pre>
</div>

## data Features

A type holding the styles, classes, attributes, properties, listeners and lifecycle methods that should be applied to a live view.

```haskell
data Features =
  Features_
    { classes    :: Set Txt
    , styles     :: Map Txt Txt
    , attributes :: Map Txt Txt
    , properties :: Map Txt Txt
    , listeners  :: [Listener]
    , lifecycles :: [Lifecycle]
    }

instance Monoid Features where
  ...
```

<div class="info">
Interaction with this structure is expected to be managed with patterns from [pure-html](/packages/pure-html/latest) and [pure-events](/packages/pure-events/latest).
</div>

## data Comp

The `Comp` data type represents stateful contexts with a suite of lifecycle methods for carefully controlling evaluation. This type is similar to the `ReactJS` `Component` class.

```haskell
ata Comp props state = Comp
  { construct    :: IO state
  , initialize   :: state -> IO state
  , initialized  :: IO ()
  , mount        :: state -> IO state
  , executing    :: state -> IO state
  , mounted      :: IO ()
  , receive      :: props -> state -> IO state
  , force        :: props -> state -> IO Bool
  , update       :: props -> state -> IO ()
  , render       :: props -> state -> View
  , updated      :: props -> state -> IO ()
  , unmounted    :: IO ()
  }
```

<div class="hide">
These fields have no meaning without an implementation. [pure-dom](/packages/pure-dom/latest) is one such implementation that adheres to the following, incomplete, specification:

* `construct` constructs an initial state

* `initialize` has no effect, but is used for server rendering in [pure-render](/packages/pure-render/latest)

* `initialized` has no effect, but is used for server rendering in [pure-render](/packages/pure-render/latest)

* `mount` evaluates after `construct`, but before an initial render

* `executing` evaluates after an initial render when the component has constructed an evaluation context and is running asynchronously, but before the component has begun handling updates. Order of execution of `executing` relative to `mounted` is undefined and likely to be non-deterministic.

* `mounted` is enqueued to be evaluated after the full `View` tree has been mounted and is run in top-down order relative to other `mounted` calls in the same `View` tree.

* `receive` is run when external properties have been received by the component. `receive` may return a new state modified with respect to the new properties.

* `force` is run when external properties have changed or state has been updated. `force` can be used to force a view to ***not*** be re-rendered. `force` defaults to `True`, meaning every property change or state update will be handled in the update loop. See the note below about intermediate updates in the render loop.

* `update` is run immediately before a components view has been reconciled with a batch of state and property updates.

* `render` is the method by which a component generates a view from its properties and state. This is a pure method that should not perform effects.

* `updated` is run immediately after a components view has been reconciled with a batch of state and property updates.

* `unmounted` is run immediately before a component has been invalidated and will no longer receive updates. The unmounting itself will happen subsequent to the invalidation and in an animation frame and possibly an idle callback.

<div class="info">
Note that [pure-dom](/packages/pure-dom/latest) may not render every intermediate view, but all intermediate states are seen during updates. See [Alan Kay](https://youtu.be/QjJaFG63Hlo?t=6929) for a theoretical understanding of the problems that the two-phase update/render loop solves. While components allow for direct assignment, abstractions like [pure-elm](/packages/pure-elm/latest) build upon the component abstraction to remediate this issue by allowing the stateful context to optionally react to received messages.
</div>

There exists a [Default](/packages/pure-default/latest) instance for `Comp`.

```haskell
instance Default (Comp props state) where
  def = Comp
    { construct   = return (error "Comp.construct: no initial state supplied.")
    , initialize  = return
    , initialized = return ()
    , mount       = return
    , executing   = return
    , mounted     = return ()
    , receive     = \_ -> return
    , force       = \_ _ -> return True
    , update      = \_ _ -> return ()
    , render      = \_ _ -> NullView Nothing
    , updated     = \_ _ -> return ()
    , unmounted   = return ()
    }
```

This default instance requires instantiation with `construct`, or an exception will be thrown.

<pre data-try>
import Pure

minimal = Component $ \_ -> 
  def { construct = pure () }

main = inject body (minimal ())
</pre>
</div>

## data ComponentPatch

Used to represent core updates to live components.

```haskell
data ComponentPatch props state
  = Unmount (Maybe View) (IO ())
  | UpdateProperties props
  | UpdateState (props -> state -> IO (state,IO ()))
```

<div class="hide">
<div class="warn">
This is an internal structure used to send updates to a component, and should not used manually. Instead, use [modifyM](Pure.Data.View/modifyM) and its derivatives, or standard declarative component views to update properties and unmount.
</div>
</div>

## data Ref

A reference to a component's evaluation context. Access to a `Ref` allows the reference holder to view the referenced component's properties with [ask](Pure.Data.View/ask), the state with [get](Pure.Data.View/get), or to send a state update with [modifyM](Pure.Data.View/modifyM) or its derivatives.

```haskell
data Ref props state
  = Ref
      { crView       :: IORef View
      , crProps      :: IORef props
      , crState      :: IORef state
      , crComponent  :: IORef (Comp props state)
      , crPatchQueue :: IORef (Maybe (Queue (ComponentPatch props state)))
      }
```

<div class="hide">
<div class="warn">
This is an internal type, and interaction with the fields of this structure are expected to be protected by being proxied through [modifyM](Pure.Data.View/modifyM) or its derivatives, [get](Pure.Data.View/get), [ask](Pure.Data.View/ask), and, rarely, [look](Pure.Data.View/look).

Using this type to modify a live component is likely to break something.
</div>
</div>


## data TypeWitness

Holds a `GHC.Fingerprint.Fingerprint` to improve the performance of comparing components. The type parameter is a phantom.

```haskell
data TypeWitness a = TypeWitness Fingerprint
```

<div class="info">
`TypeWitness` is a datatype for laziness purposes. The goal is to avoid ever calculating `Fingerprint`s, and instead use `reallyUnsafePtrEquality#` to compare the `TypeWitness`-producing thunks.

I'm not positive that this is necessary, but it seemed to work to reduce time spent in calls to the very expensive `goog.crypt.md5` in the browser.
</div>

## witness

Generates a [TypeWitness](Pure.Data.View/data%20TypeWitness) for an arbitrary `Typeable` value.

```haskell
witness :: forall a. Typeable a => TypeWitness a
```

## sameTypeWitness

The `sameTypeWitness` function compares two arbitrarily typed `TypeWitness` instances for equality. 

```haskell
sameTypeWitness :: TypeWitness a -> TypeWitness b -> Bool
```

<div class="info">
Equality implies that two components have the same type, but does not guarantee that those two components have the same implementation!  This is an important consideration for those implementing reconciliation algorithms!
</div>

## data View

The `View` data type is the unified core hierarchical type that Pure uses to implement interfaces, servers, services and computational contexts.

<div class="hide">
The `ComponentView` constructor is used to implement web-based user interfaces, servers, services and computational contexts, but the other constructors are specific to web-based user interfaces.

```haskell
data View where
  HTMLView ::
    { elementHost :: Maybe Element
    , tag         :: Txt
    , features    :: {-# UNPACK #-}!Features
    , children    :: [View]
    } -> View

  TextView ::
    { textHost :: Maybe Text
    , content  :: Txt
    } -> View

  NullView ::
    { elementHost :: Maybe Element
    } -> View

  RawView ::
    { elementHost:: Maybe Element
    , tag        :: Txt
    , features   :: {-# UNPACK #-}!Features
    , content    :: Txt
    } -> View

  SVGView ::
    { elementHost :: Maybe Element
    , tag         :: Txt
    , features    :: {-# UNPACK #-}!Features
    , xlinks      :: !(Map Txt Txt)
    , children    :: [View]
    } -> View

  KHTMLView ::
    { elementHost   :: Maybe Element
    , tag           :: Txt
    , features      :: {-# UNPACK #-}!Features
    , keyedChildren :: [(Int,View)]
    } -> View

  KSVGView ::
    { elementHost   :: Maybe Element
    , tag           :: Txt
    , features      :: {-# UNPACK #-}!Features
    , xlinks        :: !(Map Txt Txt)
    , keyedChildren :: [(Int,View)]
    } -> View

  SomeView :: Pure a =>
    { renderable :: a
    } -> View

  LazyView ::
    { lazyFun :: a -> View
    , lazyArg :: a
    } -> View

  PortalView ::
    { portalProxy :: Maybe Element
    , portalDestination :: Element
    , portalView :: View
    } -> View

  ComponentView ::
    { __comp_witness :: TypeWitness (props,state)
    , record :: Maybe (Ref props state)
    , comp   :: Ref props state -> Comp props state
    , props  :: props
    } -> View
```

* `HTMLView` is used to construct standard HTML views, like `<div>` and `<span>`.

* `KHTMLView` is used to construct standard HTML views with keyed children for improved diffing performance. However, keyed nodes are rarely necessary, even for performance.

* `SVGView` is used to construct standard SVG views. This is necessarily split from `HTMLView` because of the requirement to namespace and to improve performance of HTML rendering and diffing paths.

* `KSVGView` is used to construct standard SVG views with keyed children for improved diffing performance. However, keyed nodes are rarely necessary, even for performance.

* `NullView` is used to construct a hole in a view. In html, the `NullView` will render as a `<template>` node. The `NullView` is often used for conditional rendering.

* `RawView` is used to inject raw markup into a dom node. This view can be unsafe! Be sure not to inject arbitrary content from unknown sources!

* `PortalView` is used to embed the view in an out-of-tree location while maintaining the view in-line. That is, the view will still be unmounted as expected if the container is unmounted, but is rendered somewhere else in the tree. In html, the portal origin will render as a `<template>` node, as a placeholder, in case it needs to be replaced.

* `TextView` represents a text node.

* `SomeView` represents an un-rendered data-oriented view. This can allow a view to short-circuit on the data that constructs a view rather than the constructed view. This approach is not generally necessary, since [pure-dom](/packages/pure-dom/latest) is exceptionally good at short-circuiting on equivalent render thunks.

* `LazyView` is a reification of the idea of data + render method by pairing data with its renderer. This approach does not currently work and is, in general, unnecessary.

* `ComponentView` is a wrapper to construct a `View` from a function that will construct a [Comp](Pure.Data.View/data%20Comp), given that component's reference. This is how a component acquires a reference to itself for induction of state updates. When the properties (`props`) used to construct a `ComponentView` change, they are injected into the component's evaluation context for integration.

There is a [Default](/packages/pure-default/latest) instance for `View`.

```haskell
instance Default View where
  def = NullView Nothing
```

There is an `IsString` instance for `View`.

```haskell
instance IsString View where
  fromString = TextView Nothing . toTxt
```

Similarly, there is a `FromTxt` instance for `View`.

```haskell
instance FromTxt View where
  fromTxt = TextView Nothing
```
</div>

## class Pure

The `Pure` class enriches data with a standard rendering method.

```haskell
class Pure a where
  view :: a -> View
```

<div class="hide">
<div class="info">
Any type may be rendered via an arbitrary functional evaluation, but `Pure` enables promotion of a functional evaluation to a standardization. Thus, the `Pure` class can help to encourage the implementation of data-oriented views and interfaces, in some contexts.
</div>

<pre data-try>
import Pure

data Person = Person 
  { name :: Txt
  , age :: Int
  }

instance Pure Person where
  view Person {..} =
    fromTxt $
      name <> " is " <> toTxt age <> " years old."

bob = Person "Bob" 42

main = inject body (view bob)
</pre>

<pre data-try>
import Pure

data Tagged = forall a. Pure a => Tagged 
  { name :: Txt
  , tagged :: a 
  }

instance Pure Tagged where
  view Tagged {..} = 
    Div <||> 
      [ txt name
      , View tagged
      ]

a :: Tagged
a = Tagged "a" ("Some Text" :: View)

b :: Tagged
b = Tagged "b" a

main = inject body (view b)
</pre>
</div>

## pattern View

The `View` pattern will construct or match a `View` for any type implementing the `Pure` class.

```haskell
pattern View :: Pure a => a -> View
```

<div class="hide">
<pre data-try>
import Pure

data Person = Person { name :: Txt, age :: Int }

instance Pure Person where
  view Person {..} = fromTxt $ 
    name <> " is " <> toTxt age <> " years old."

birthday :: View -> View
birthday (View Person { age = a, ..}) = 
  let age = a + 1 
  in View Person {..}
birthday v = v

bob :: View
bob = View (Person "Bob" 41)

main = inject body (birthday bob)
</pre>
</div>

## get

Retrieve the current state of a component from a reference.

```haskell
get :: Ref props state -> IO state
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Data.Render
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body (counter 10)

counter :: Int -> View
counter = Component $ \self ->
  let increment = modify_ self $ \_ -> succ
  in def
    { construct = ask self
    , render = \_ c ->
      Div <||>
        [ Button <| OnClick (\_ -> increment) |> [ "Increment" ] 
        , Button <| OnClick (\_ -> get self >>= print) |> [ "Print Current Count" ]
        , txt c
        ]
    }
</pre>
</div>

## ask

Retrieve the current properties of a component from a reference.

```haskell
ask :: Ref props state -> IO props
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Data.Render
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body (counter 10)

counter :: Int -> View
counter = Component $ \self ->
  let increment = modify_ self $ \_ -> succ
  in def
    { construct = ask self
    , render = \_ c ->
      Div <||>
        [ Button <| OnClick (\_ -> increment) |> [ "Increment" ] 
        , Button <| OnClick (\_ -> ask self >>= print) |> [ "Print Properties" ]
        , txt c
        ]
    }
</pre>
</div>

## look

Allows for introspection into the current rendered view of a component. 

```haskell
look :: Ref props state -> IO View
```

<div class="hide">
<div class="warn">
This method is extremely unsafe and use of it is likely evidence that the application should be restructured.

The referenced view is subject to change at any time, even while being inspected. It is even possible for the retrieved view to disappear or be replaced entirely. Consider this a snapshot, only meant for analysis.
</div>

<pre data-try>
import Pure
import Pure.Data.Render
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body (counter ())

counter = Component $ \self ->
  let reify = do
        v <- look self
        print (show v)
  in def
    { construct = pure (0 :: Int)
    , render = \_ c ->
      Div <||>
        [ Button <| OnClick (\_ -> reify) |> [ "Reify" ] 
        , txt c
        ]
    }
</pre>
</div>

## modify

A restricted version of [modifyM](Pure.Data.View/modifyM), specialized to pure state updates. If `True` is returned, the component is still receptive to updates.

```haskell
modify :: Ref props state -> (props -> state -> state) -> IO Bool
```

<div class="hide">
<pre data-try>
import Pure
import Control.Concurrent
import Control.Monad
import Data.Function

main = inject body (counter ())

counter = Component $ \self -> 
  let
    increment = modify self $ \_ c -> succ c
  in def
    { construct = do
      forkIO $ fix $ \loop -> do
        delay Second 
        alive <- increment
        when alive loop
      pure (0 :: Int)
    , render = \_ -> txt
    }
</pre>

<div class="info">
In [pure-dom](/packages/pure-dom/latest) the following apply:

  * The callback will be run batched, with other updates, in the current or next component update cycle.
  * Since updates are batched together, the state returned by any one `modifyM` might never be projected into a rendered view. 
</div>
</div>

## modify_

A restricted version of [modifyM](Pure.Data.View/modifyM), specialized to pure state updates that ignores the returned status `Bool`. The callback passed to `modify_` has access to both the current props and the current state.

```haskell
modify_ :: Ref props state -> (props -> state -> state) -> IO ()
```

<div class="hide">
<pre data-try>
import Pure

main = inject body (counter ())

counter = Component $ \self -> 
  let
    increment = modify_ self $ \_ -> succ
  in def
    { construct = pure (0 :: Int)
    , render = \_ c -> 
      Div <||>
        [ Button <| OnClick (\_ -> increment) |> [ "Increment" ]
        , txt c
        ]
    }
</pre>

<div class="info">
In [pure-dom](/packages/pure-dom/latest) the following apply:

  * The callback will be run batched, with other updates, in the current or next component update cycle.
  * Since updates are batched together, the state returned by any one `modifyM` might never be projected into a rendered view. 
  * If the method returns `True`, the component was still alive.
</div>
</div>

## modifyM

Queues a state update method that returns a callback to be run after the update has been applied. This method returns `True` if the component is still receptive to updates.

```haskell
modifyM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
```

<div class="hide">
<pre data-try>
import Pure
import Control.Concurrent
import Control.Monad
import Data.Function
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body (clock ())

clock = Component $ \self -> 
  let
    tick = modifyM self $ \_ _ -> do
      print "During tick."
      now <- time
      pure (now,print "After tick.")
  in def
    { construct = do
      forkIO $ fix $ \loop -> do
        delay Second
        print "Before tick."
        alive <- tick
        print "After queueing tick."
        when alive loop
      time
    , render = \_ -> toDateTime
    }
</pre>

<div class="info">
In [pure-dom](/packages/pure-dom/latest) the following apply:

  * The callback will be run batched, with other updates, in the current or next component update cycle
  * Since updates are batched together, the state returned by any one `modifyM` might never be projected into a rendered view.
  * Calling [get](Pure.Data.View/get) in the callback returned from an update function is guaranteed to see either the state returned from the update or a newer state.
  * If the method returns `True`, the component was still alive.
</div>
</div>

## modifyM_

A version of [modifyM](Pure.Data.View/modifyM) that ignores the returned status `Bool`.

Caveats associated with [modifyM](Pure.Data.View/modifyM) apply.

```haskell
modifyM_ :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO ()
```

<div class="hide">
<pre data-try>
import Pure
import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body (clock ())

clock = Component $ \self -> 
  let
    tick = do
      modifyM_ self $ \_ _ -> do
        print "During tick."
        now <- time
        pure (now,print "After tick.")
  in def
    { construct = do
      forkIO $ forever $ do
        delay Second
        print "Before queueing tick."
        tick
      time
    , render = \_ -> toDateTime
    }
</pre>

<div class="info">
In [pure-dom](/packages/pure-dom/latest) the following apply:

  * The callback will be run batched, with other updates, in the current or next component update cycle.
  * Since updates are batched together, the state returned by any one `modifyM_` might never be projected into a rendered view, even though it is applied. 
  * Calling [get](Pure.Data.View/get) in the callback returned from an update function is guaranteed to see either the state returned from the update or a newer state.
</div>
</div>

## setProps

Inject a new version of properties into a component. Use of this method is likely evidence that the application should be restructured or reconsidered, as properties should be implicitly injected through a declarative interface. If `True` is returned, the component is still receptive to updates.

```haskell
setProps :: Ref props state -> props -> IO Bool
```

<div class="hide">
<div class="warn">
Use of this method is likely evidence that an application or component should be restructed or reconsidered, as updates should be managed by implicit property changes in declarative interfaces, or calls to [modifyM](Pure.Data.View/modifyM), or derivatives thereof.
</div>
</div>

## queueComponentUpdate

The `queueComonentUpdate` method is used internally by [pure-dom](/packages/pure-dom/latest) to send a state update method to a component for integration. If `True` is returned, the component is still receptive to updates.

```haskell
queueComponentUpdate :: Ref props state -> ComponentPatch props state -> IO Bool
```

<div class="hide">
<div class="warn">
Use of this method is likely evidence that an application or component should be restructed or reconsidered, as updates should be managed by implicit property changes in declarative interfaces, or calls to [modifyM](Pure.Data.View/modifyM), or derivatives thereof.
</div>
</div>

## getHost

The `getHost` method is an internal method for extracting the mounting context of a node. This method is unsafe.

```haskell
getHost :: View -> Maybe Node
```