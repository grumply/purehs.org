# pure-core

> #### Warning 
>
> This is an internal package. This is not the best place to learn how to develop with Pure. Consider starting with the [Basics Tutorial](/tut/basics).
> 
> Importantly, [Pure.Data.View](latest/Pure.Data.View) contains methods and types that are internal to Pure, but [Pure.Data.View.Patterns](latest/Pure.Data.View.Patterns) contains patterns and classes that are commonly used to construct [Views](latest/Pure.Data.View/data%20View).

This package implements low-level types and methods for constructing and manipulating dependent and hierarchical computational contexts. When implementing user interfaces, the dependent evaluation is of nested and hierarchical views with shared state and environment. When implementing servers and services, the dependent evaluation is of computational contexts with shared state and environment.

These types and methods are used to implement higher-level abstractions for server, service, and interface development. See, for example, [pure-html](/doc/pure-html/latest), [pure-svg](/doc/pure-svg/latest), and [pure-events](/doc/pure-events/latest). For a driver that manages instances of these types, see [pure-dom](/doc/pure-dom/latest).

## Pure.Data.View

### data Target

A type used to tag event listeners with their binding destination. Listeners may be bound to the local target element, the global window, or the global document. This type is generally not exposed to the user, instead relying on the patterns developed in [pure-events](/doc/pure-events/latest) to instantiate targeted listeners.

```haskell
data Target
  = ElementTarget
  | WindowTarget
  | DocumentTarget
  deriving Eq
```

### data Listener

A type representing event listeners to be applied to [Views](Pure.Data.View/data%20View).

> NOTE: [Options](/doc/pure-lifted/latest/Pure.Lifted/data%20Options) and [Evt](/doc/pure-lifted/latest/Pure.Lifted/data%20Evt) are defined in [pure-lifted](/doc/pure-lifted/latest).

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

A type representing low-level access to context during, or immediately following, [View](Pure.Data.View/data%20View) construction. This type is often necessary for constructing complex animated user interfaces.

```haskell
data Lifecycle =
  HostRef
    { withHost :: Node -> IO () }
```

### data Features

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

<div class="hide">
These fields have no meaning without an implementation. [pure-dom](/doc/pure-dom/latest) is one such implementation that adheres to the following specification:

* `construct` constructs an initial state

* `initialize` has no effect, but is used for server rendering in [pure-render](/doc/pure-render/latest)

* `initialized` has no effect, but is used for server rendering in [pure-render](/doc/pure-render/latest)

* `mount` evaluates after `construct`, but before an initial render

* `executing` evaluates after an initial render when the component has constructed an evaluation context and is running asynchronously, but before the component has begun handling updates. Order of execution of `executing` relative to `mounted` is undefined and likely to be non-deterministic.

* `mounted` is enqueued to be evaluated after the full `View` tree has been mounted and is run in top-down order relative to other `mounted` calls in the same `View` tree.

* `receive` is run when external properties have been received by the component. `receive` may return a new state modified with respect to the new properties.

* `force` is run when external properties have changed or state has been updated. `force` can be used to force a view to ***not*** be re-rendered. `force` defaults to `True`, meaning every property change or state update will be handled in the update loop. See the note below about intermediate updates in the render loop.

* `update` is run immediately before a components view has been reconciled with a batch of state and property updates.

* `render` is the method by which a component generates a view from its properties and state. This is a pure method that should not perform effects.

* `updated` is run immediately after a components view has been reconciled with a batch of state and property updates.

* `unmounted` is run immediately before a component has been invalidated and will no longer receive updates. The unmounting itself will happen subsequent to the invalidation and in an animation frame and possibly an idle callback.

> Note that [pure-dom](/doc/pure-dom/latest) may not render every intermediate view, but all intermediate states are seen during updates. See [Alan Kay on assignment](https://softwareengineering.stackexchange.com/questions/81197/what-did-alan-kay-mean-by-assignment-in-the-early-history-of-smalltalk) for a theoretical understanding of the problems that the two-phase update/render loop solves. While components allow for direct assignment, abstractions like [pure-elm](/doc/pure-elm/latest) build upon the component abstraction to remediate this issue by allowing the stateful context to optionally react to received messages.

There exists a [Default](/doc/pure-default/latest) instance for `Comp`.

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

```haskell
myMinimalComponent = def { construct = ... }
```
</div>

### data ComponentPatch

Used to represent core updates to live components.

```haskell
data ComponentPatch props state
  = Unmount (Maybe View) (IO ())
  | UpdateProperties props
  | UpdateState (props -> state -> IO (state,IO ()))
```

### data Ref

A reference to a component's evaluation context. Access to a `Ref` allows the reference holder to view the referenced component's properties with `ask`, the state with `get`, or to send a state update with `modify`.

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

Holds a `GHC.Fingerprint.Fingerprint` to improve the performance of comparing components. The type parameter is a phantom.

```haskell
data TypeWitness a = TypeWitness Fingerprint
```

> #### Note
>
> `TypeWitness` is a datatype for laziness purposes. The goal is to avoid ever calculating `Fingerprint`s, and instead use `reallyUnsafePtrEquality#` to compare the `TypeWitness`-producing thunks.
> 
> I'm not positive that this is necessary, but it seemed to work to reduce time spent in calls to the very expensive `goog.crypt.md5` in the browser.

### witness

Generates a [TypeWitness](Pure.Data.View/data%20TypeWitness) for an arbitrary `Typeable` value.

```haskell
witness :: forall a. Typeable a => TypeWitness a
```

### sameTypeWitness

The `sameTypeWitness` function compares two arbitrarily typed `TypeWitness` instances for equality. 

> #### Note
>
> Equality implies that two components have the same type, but does not guarantee that those two components have the same implementation!  This is an important consideration for those implementing reconciliation algorithms!

```haskell
sameTypeWitness :: TypeWitness a -> TypeWitness b -> Bool
```

### data View

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

* `PortalView` is used to embed the view in an out-of-tree location while maintaining the view in-line. That is, the view will still be unmounted as expected if the container is unmounted, but is rendered somewhere else in the tree. In html, the portal origin will render as a `<template>` node, in case it needs to be replaced.

* `TextView` represents a text node.

* `SomeView` represents an un-rendered data-oriented view. This can allow a view to short-circuit on the data that constructs a view rather than the constructed view. This approach is not generally necessary, since [pure-dom](/doc/pure-dom/latest) is exceptionally good at short-circuiting on equivalent render thunks.

* `LazyView` is a reification of the idea of data + render method by pairing data with its renderer.

* `ComponentView` is a wrapper to construct a `View` from a function that will construct a `Comp`, given that component's reference. This is how a component acquires a reference to itself for induction of state updates. When the properties (`props`) used to construct a `ComponentView` change, they are injected into the component's evaluation context for integration.

There is a [Default](/doc/pure-default/latest) instance for `View`.

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

### class Pure

The `Pure` class enriches data with a standard rendering method.

```haskell
class Pure a where
  view :: a -> View
```

> #### Note
> 
> Any type may be rendered via an arbitrary functional evaluation, but `Pure` enables promotion of a functional evaluation to a standardization. Thus, the `Pure` class can help to encourage the implementation of data-oriented views and interfaces, in some contexts.

### pattern View

The `View` pattern will construct or match a `View` for any type implementing the `Pure` class.

```haskell
pattern View :: Pure a => a -> View
```

### get

Retrieve the current state of a component from a reference.

```haskell
get :: Ref props state -> IO state
```

### ask

Retrieve the current properties of a component from a reference.

```haskell
ask :: Ref props state -> IO props
```

### look

Allows for introspection into the current rendered view of a component. 

> ## WARNING
> 
> This method is extremely unsafe and use of it is likely evidence that the application should be restructured.
>
> The referenced view is subject to change at any time, even while being inspected. It is even possible for the retrieved view to disappear or be replaced entirely. Consider this a snapshot, only.

```haskell
look :: Ref props state -> IO View
```

### modify

A restricted version of [modifyM](Pure.Data.View/modifyM), specialized to pure state updates. 

Caveats associated with [modifyM](Pure.Data.View/modifyM) apply.

```haskell
modify :: Ref props state -> (props -> state -> state) -> IO Bool
```

### modify_

A restricted version of [modifyM](Pure.Data.View/modifyM), specialized to pure state updates and ignoring the returned status `Bool`.

Caveats associated with [modifyM](Pure.Data.View/modifyM) apply.

```haskell
modify_ :: Ref props state -> (props -> state -> state) -> IO ()
```

### modifyM

Queues a state update method that returns a callback to be run after the update has been applied. This method returns `True` if there are no other updates pending.

```haskell
modifyM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
```

<div class="hide">
> #### Notes
> 
> In [pure-dom](/doc/pure-dom/latest) the following apply:
>   * the callback will be run, batched with other updates, in the current or next component update cycle
>   * calling `get` in the callback returned from an update function is guaranteed to see that state or a newer state
>   * if the method returns `True`, the update has scheduled the thread with the RTS to wake up and perform an update.
>   * since updates are batched together, the state returned by any one `modifyM` might never be projected into a rendered view. 
</div>

### modifyM_

A version of [modifyM](Pure.Data.View/modifyM) that ignores the returned status `Bool`.

Caveats associated with [modifyM](Pure.Data.View/modifyM) apply.

```haskell
modifyM_ :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO ()
```

### setProps

Inject a new version of properties into a component. Use of this method is likely evidence that the application should be restructured or reconsidered, as properties should be implicitly injected through a declarative interface. If `True` is returned, other updates are already pending.

```haskell
setProps :: Ref props state -> props -> IO Bool
```

### queueComponentUpdate

The `queueComonentUpdate` method is used internally by [pure-dom](/doc/pure-dom/latest) to send a state update method to a component for integration. 

> #### Warning
>
> Use of this method is likely evidence that the application should be restructed or reconsidered, as updates should be managed by implicit property changes in declarative interfaces, or calls to [modifyM](Pure.Data.View/modifyM), or derivatives thereof.

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

<div class="hide">
```haskell
user :: User -> View
user = Component $ \self ->
  def
    { ...
    }
```
</div>

### pattern Null

A null view pattern for conditonal rendering of views.

```haskell
pattern Null :: View
```

<div class="hide">
```haskell
activeUser :: Maybe Username -> View
activeUser Nothing = Null
activeUser (Just u) = {...}
```
</div>

### pattern Raw

A pattern for unsafe construction of an HTML view from a text value.

> WARNING: `Raw` does not sanitize its input!

```haskell
pattern Raw :: Txt -> View
```

<div class="hide">
```haskell
rawH1 = Raw "<h1>Raw H1</h1>"
```
</div>

### pattern Keyed

A pattern for converting a non-keyed view to a keyed variant. 

> #### Note
>
> This pattern only works on `HTML` and `SVG` views. 
>
> Use `(|#>)` or `(<||#>)` to add keyed children to a keyed view.

```haskell
pattern Keyed :: View -> View
```

<div class="hide">
```haskell
keyedDiv :: View
keyedDiv = Keyed Div
```
</div>

### pattern Class

A pattern for adding a class to a view. 

> #### Note
>
> This method only works on HTML and SVG nodes and Portals thereof. For non-HTML and non-SVG nodes, it sets nothing.
>
> Empty class strings are filtered out during rendering in [pure-dom](/doc/pure-dom/latest).
>
> #### Warning
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Classes) and `Data.List.elem` for matching.

```haskell
pattern Class :: HasFeatures a => Txt -> a -> a
```

<div class="hide">
```haskell
blue :: View
blue = Div <| Class "blue"
```
</div>

### pattern Classes

A pattern for viewing or overwriting classes on a view.

```haskell
pattern Classes :: HasFeatures a => [Txt] -> a -> a
```

> #### Note
>
> This method only works on HTML and SVG nodes and Portals thereof. For non-HTML and non-SVG nodes, it sets nothing and only views an empty list.
>
> Empty class strings are filtered out during rendering in [pure-dom](/doc/pure-dom/latest), classes with spaces are broken up into multiple classes when rendered.

<div class="hide">
```haskell
container :: HasFeatures a => a -> a
container = Classes ["wrapped","content"]
```
</div>

### pattern Style

A pattern for adding a style to a view.

> #### Note
>
> This method only works on HTML and SVG nodes and Portals thereof. 

> #### Warning
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Styles) and `Data.List.lookup` for matching.

```haskell
pattern Style :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
```haskell
myView = Div <| Style "width" "100%"
```
</div>

### pattern Property

A pattern for adding a property to a view.

> #### Note
>
> This method only works on HTML and SVG nodes. 

> #### Warning
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Properties) and `Data.List.lookup` for matching.

```haskell
pattern Property :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
```haskell
myView val = Input <| Property "value" val
```
</div>

### pattern Attribute

A pattern for adding an attribtue to a view.

> #### Note
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Attributes) and `Data.List.lookup` for matching.

```haskell
pattern Attribute :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
```haskell
myView = Input <| Attribute "id" "myView"
```
</div>

### <|

Transform a type into a view-able value or a transformation to an already view-able type.

```haskell
infixl 8 <|
(<|) :: ToView b => a -> (a -> b) -> View
```

<div class="hide">
```haskell
Div <| Class "green" . Id "green-div"
```
</div>

### |>

Set children on a value supporting [HasChildren](Pure.Data.View.Patterns/class%20HasChildren). Note that unlike other setters, this children setter does not convert the value to a [View](Pure.Data.View/data%20View). This operator has a very high right fixity.

```haskell
infixr 9 |>
(|>) :: (ToView b, HasChildren a) => (a -> a) -> [b] -> a -> a
```

<div class="hide">
```haskell
Div <| Class "greeting" |>
  [ Span <| Class "hello" |> [ "Hello" ]
  , Span <| Class "world" |> [ ", World!" ]
  ]
```
</div>

### <||>

Specify children on a value supporting [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), and then convert the value to a [View](Pure.Data.View/data%20View).

```haskell
(<||>) :: (ToView a, ToView b, HasChildren a) => a -> [b] -> View
```

<div class="hide">
```haskell
Div <||>
  [ Span <||> [ "Hello" ]
  , Span <||> [ ", World!" ]
  ]
```
</div>

### |#>

Set keyed children on a value supporting [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren). Note that unlike some other setters, this keyed children setter does not convert the value to a [View](Pure.Data.View/data%20View). This operator has a very high right fixity.

```haskell
infixr 9 |#>
(|#>) :: (HasKeyedChildren a, ToView b) => (a -> a) -> [(Int,b)] -> a -> a
```

<div class="hide">
```haskell
users us =
  Keyed Div <| Class "users" |#>
    [ (hash u, View u)
    | u <- us
    ]
```
</div>

### <||#>

Specify keyed children without applying attributes, properties, listeners, or lifecycle methods. Must be paired with `Keyed` or used on views supporting `HasKeyedChildren`.

```haskell
(<||#>) :: (ToView a, ToView b, HasKeyedChildren a) => a -> [(Int,b)] -> View
```

<div class="hide">
```haskell
users us =
  Keyed Div <||#>
    [ (hash u, u)
    | u <- us
    ] -- fmap (hash &&& id) us
```
</div>

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
