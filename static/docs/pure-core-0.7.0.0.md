# pure-core

> NOTE: This is an internal package implementing core types, methods, and patterns. This is not the best place to learn how to develop with Pure. Consider starting with the [Basics Tutorial](/tut/basics).

This package implements low-level types and methods for constructing and manipulating dependent and hierarchical computational contexts. When implementing user interfaces, the dependent evaluation is of nested and hierarchical views with shared state and environment. When implementing servers and services, the dependent evaluation is of computational contexts with shared state and environment.

These types and methods are used to implement higher-level abstractions that ease server, service, and interface implementation. See, for example, [pure-html](/doc/pure-html/0.7.0.0), [pure-svg](/doc/pure-svg/0.7.0.0), and [pure-events](/doc/pure-html/0.7.0.0).

## Pure.Data.View

### data Target

The `Target` data type is used to tag event listeners with their binding destination. Listeners may be bound to the local target element, the global window, or the global document.

```haskell
data Target
  = ElementTarget
  | WindowTarget
  | DocumentTarget
  deriving Eq
```

### data Listener
The `Listener` data type represents an event listener for use with javascript interfaces.

> NOTE: `Options` and `Evt` are defined in [pure-lifted](/doc/pure-lifted/0.7.0.0).

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

### data Lifecycle

The `Lifecycle` data type represents low-level access to context during, or immediately following, `View` construction. This type is often necessary for constructing complex user interfaces.

```haskell
data Lifecycle =
  HostRef
    { withHost :: Node -> IO () }
```

### data Features

The `Features` data type holds the styles, classes, attributes, properties, listeners and lifecycle methods that should be applied to a view.

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

### data Comp

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

These fields have no meaning without an implementation. [pure-dom](/doc/pure-dom/0.7.0.0) is one such implementation that adheres to the following specification:

* `construct` constructs an initial state

* `initialize` has no effect, but is used for server rendering in [pure-render](/doc/pure-render/0.7.0.0)

* `initialized` has no effect, but is used for server rendering in [pure-render](/doc/pure-render/0.7.0.0)

* `mount` evaluates after `construct`, but before an initial render

* `executing` evaluates after an initial render when the component has constructed an evaluation context and is running asynchronously, but before the component has begun handling updates. Order of execution of `executing` relative to `mounted` is undefined and likely to be non-deterministic.

* `mounted` is enqueued to be evaluated after the full `View` tree has been mounted and is run in top-down order relative to other `mounted` calls in the same `View` tree.

* `receive` is run when external properties have been received by the component. `receive` may return a new state modified with respect to the new properties.

* `force` is run when external properties have changed or state has been updated. `force` can be used to force a view to ***not*** be re-rendered. `force` defaults to `True`, meaning every property change or state update will be handled in the update loop. See the note below about intermediate updates in the render loop.

* `update` is run immediately before a components view has been reconciled with a batch of state and property updates.

* `render` is the method by which a component generates a view from its properties and state. This is a pure method that should not perform effects.

* `updated` is run immediately after a components view has been reconciled with a batch of state and property updates.

* `unmounted` is run immediately before a component has been invalidated and will no longer receive updates. The unmounting itself will happen subsequent to the invalidation and in an animation frame and possibly an idle callback.

> Note that [pure-dom](/doc/pure-dom/0.7.0.0) may not render every intermediate view, but all intermediate states are seen during updates. See [Alan Kay on assignment](https://softwareengineering.stackexchange.com/questions/81197/what-did-alan-kay-mean-by-assignment-in-the-early-history-of-smalltalk) for a theoretical understanding of the problems that the two-phase update/render loop solves. While components allow for direct assignment, abstractions like [pure-elm](/doc/pure-elm/0.7.0.0) build upon the component abstraction to remediate this issue by allowing the stateful context to optionally react to received messages.


There exists a [Default](/doc/pure-default/0.7.0.0) instance for `Comp`.

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

This instance requires instantiation of `construct`.

```haskell
myComponent = def { construct = ... }
```

### data ComponentPatch

The `ComponentPatch` is a data type used by [pure-dom](/doc/pure-dom/0.7.0.0) to represent updates to a component that are generated implicitly from `props` changing, or from a call to `modify` a component's state, or from a component being unmounted.

```haskell
data ComponentPatch props state
  = Unmount (Maybe View) (IO ())
  | UpdateProperties props
  | UpdateState (props -> state -> IO (state,IO ()))
```

### data Ref

The `Ref` data type is a reference to a component's evaluation context. Access to a `Ref` allows the reference holder to view the referenced component's properties with `ask`, the state with `get`, or to send a state update with `modify`.

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

### data TypeWitness

The `TypeWitness` type holds a `Fingerprint` from `GHC.Fingerprint` to improve the performance of diffing components. The type variable to `TypeWitness` is a phantom type.

```haskell
data TypeWitness a = TypeWitness Fingerprint
```

### witness

The `witness` function generates a `TypeWitness` for an arbitrary `Typeable a`.

```haskell
witness :: forall a. Typeable a => TypeWitness a
```

### sameTypeWitness

The `sameTypeWitness` function compares two arbitrarily typed `TypeWitness` instances for equality that is tuned for fast diffing.

```haskell
sameTypeWitness :: TypeWitness a -> TypeWitness b -> Bool
```

### data View

The `View` data type is the core unified hierarchical type that Pure uses to implement interfaces, servers, services and computational contexts.

The `ComponentView` constructor is useful for interfaces, servers, services and computational contexts, but the other constructors are specific to browser interfaces.

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

* `PortalView` is used to embed the view in an out-of-tree location while maintaining the view in-line. That is, the view will still be unmounted as expected if the container is unmounted, but is rendered somewhere else in the tree. In html, the portal origin will render as a `<template>` node, in case it needs to be replaced.

* `TextView` represents a text node.

* `SomeView` represents an un-rendered data-oriented view. This can allow a view to short-circuit on the data that constructs a view rather than the constructed view. This approach is not generally necessary, since [pure-dom](/doc/pure-dom/0.7.0.0) is exceptionally good at short-circuiting on equivalent render thunks.

* `LazyView` is a reification of the idea of data + render method by pairing data with its renderer.

* `ComponentView` is a wrapper to construct a `View` from a function that will construct a `Comp`, given that component's reference. This is how a component acquires a reference to itself for induction of state updates. When the properties (`props`) used to construct a `ComponentView` change, they are injected into the component's evaluation context for integration.

There is a [Default](/doc/pure-default/0.7.0.0) instance for `View`.

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

### class Pure

The `Pure` class enriches data with a standard rendering method.

```haskell
class Pure a where
  view :: a -> View
```

Any type may be rendered via an arbitrary functional evaluation, but `Pure` enables promotion of a functional evaluation to a standardization. Thus, the `Pure` class can help to encourage the implementation of data-oriented views and interfaces.

### pattern View

The `View` pattern will construct or match a `View` for any type implementing the `Pure` class.

```haskell
pattern View :: Pure a => a -> View
```

### get

`get` gets the current state of a component from a reference to a component.

```haskell
get :: Ref props state -> IO state
```

### ask

`ask` views the current properties of a component from a reference to a component.

```haskell
ask :: Ref props state -> IO props
```

### look

`look` allows for introspection of a component's rendered view. This method is extremely unsafe and use of it is likely evidence that the application should be restructured.

```haskell
look :: Ref props state -> IO View
```

### modify

`modify` enqueues a pure update to a component's state. The callback passed to `modify` will be run in the component's next update cycle. `modify` returns `True` if there are no other component updates pending.

```haskell
modify :: Ref props state -> (props -> state -> state) -> IO Bool
```

> NOTE: Updates are batched together. As a result, the state returned by any one `modify` might never be rendered.

### modify_

`modify_` is the same as `modify`, above, but ignores the returned `Bool`.

### modifyM

`modifyM` is a more general versions of `modify` that allows you to effectfully modify the state of a component as well as return a callback that is run after the state has been updated. `modifyM` returns `True` if there are no other component updates pending.

```haskell
modifyM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
```

> NOTE: Updates are batched together.
>
> Calling `get` in the callback returned from an update function in `modifyM` is not guaranteed to see the state returned from the update function - it is, however, guaranteed to see that state or a newer state.
>
> Due to batching, it is possible to make changes to the state of a component that will never be rendered. To avoid this scenario, you can nest calls to `modifyM` to throttle updates:
>
> ```haskell
> modifyM ref $ \props state -> do
>   state' <- someStateUpdate state
>   return (state',modifyM_ ref $ \props state -> do
>     state' <- someThrottledStateUpdate state
>     return (state',return ())
>     )
> ```
>
> Nested calls to `modifyM` are guaranteed not to run in the same component update cycle.
>

### modifyM_

`modifyM_` is the same as `modifyM`, above, but ignores the returned `Bool`.

### setProps

`setProps` is an internal method of injecting a properties update into a component. Use of this method is likely evidence that the application should be restructured or reconsidered. If `True`, the returned `Bool` indicates that other updates are already pending.

```haskell
setProps :: Ref props state -> props -> IO Bool
```

### queueComponentUpdate

The `queueComonentUpdate` method is used internally by [pure-dom](/doc/pure-dom/0.7.0.0) to send a state update method to a component for integration.

```haskell
queueComponentUpdate :: Ref props state -> ComponentPatch props state -> IO Bool
```

### getHost

The `getHost` method is an internal method for extracting the mounting context of a node. This method is unsafe.

```haskell
getHost :: View -> Maybe Node
```

## Pure.Data.View.Patterns

`Pure.Data.View.Patterns` exports patterns, classes, and combinators for `View` construction.

### pattern Component

A view pattern for construction of components.

```haskell
pattern Component :: (Ref props state -> Comp props state) -> props -> View
```

```haskell
user :: User -> View
user = Component $ \self ->
  def
    { ...
    }
```

### pattern Null

A null view pattern for conditonal rendering of views.

```haskell
activeUser :: Maybe Username -> View
activeUser Nothing = Null
activeUser (Just u) = {...}
```

### pattern Raw

A pattern for unsafe construction of an HTML view from a text value.

> WARNING: `Raw` does not sanitize its input!

```haskell
rawH1 = Raw "<h1>Raw H1</h1>"
```

### pattern Keyed

A pattern for creating a keyed variant of an `HTML` view or an `SVG` view. Use `(|#>)` or `(<||#>)` to add keyed children to a keyed view.

```haskell
keyedDiv :: View
keyedDiv = Keyed Div
```

### pattern Class

A pattern for adding a class to a view. Note that empty class strings are filtered out during rendering.

```haskell
myView = Div <| Class "someClass"
```

### pattern Style

A pattern for adding a style to a view.

```haskell
myView = Div <| Style "width" "100%"
```

### pattern Property

A pattern for adding a property to a view.

```haskell
myView val = Input <| Property "value" val
```

### pattern Attribute

A pattern for adding an attribtue to a view.

```haskell
myView = Input <| Attribute "id" "myView"
```

### pattern <|

Apply the modification on the right before calling `toView`. If the argument on the left is a `View`, it is equivalent to calling `id`. If it is an instance of `Pure`, it is equivalent to wrapping with the `View` pattern.

```haskell
Div <| Class "green" . Id "green-div"
```

### pattern |>

Specify children after applying properties.

```haskell
Div <| Class "greeting" |>
  [ Span <| Class "hello" |> [ "Hello" ]
  , Span <| Class "world" |> [ ", World!" ]
  ]
```

### pattern <||>

Specify children without applying any attributes, properties, listeners, or lifecycle methods.

```haskell
Div <||>
  [ Span <||> [ "Hello" ]
  , Span <||> [ ", World!" ]
  ]
```

### pattern |#>

Specify keyed children after applying attributs, properties, listeners, or lifecycle methods. Must be paired with `Keyed` or used with views supporting `HasKeyedChildren`.

```haskell
users us =
  Keyed Div <| Class "users" |#>
    [ (hash u, View u)
    | u <- us
    ]
```

### pattern <||#>

Specify keyed children without applying attributes, properties, listeners, or lifecycle methods. Must be paired with `Keyed` or used on views supporting `HasKeyedChildren`.

```haskell
users us =
  Keyed Div <||#>
    [ (hash u, View u)
    | u <- us
    ]
```

### text

Inject a textual value into a `View`.

```haskell
text :: IsTxt a => a -> View
```

### txt

A specialized version of `text` for `Txt`.

```haskell
txt :: Txt -> View
```
