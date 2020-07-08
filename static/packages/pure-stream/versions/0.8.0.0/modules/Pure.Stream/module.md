
## data Streamer

The configuration type for creating a streaming view. Implements [Default](/packages/pure-default/0.8.0.0/Pure.Data.Default/class%20Default), [HasChildren](/packages/pure-core/0.8.0.0/Pure.Data.View.Patterns/class%20HasChildren), and [HasFeatures](/packages/pure-core/0.8.0.0/Pure.Data.View.Patterns/class%20HasFeatures).

```haskell
data Streamer a = Streamer
  { producer :: Stream IO a
  , consumer :: Stream IO a -> [View]
  , features :: Features
  , children :: [View]
  }
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Stream

fibs :: Stream IO Integer
fibs = unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 in more f (f1,f)

paragraph :: Integer -> View
paragraph n = P <||> [ txt n ]

main = inject body $ stream $ 
  frameStepper (100px) Streamer
    { producer = chunksOf 10 (fmap paragraph fibs)
    , consumer = toList
    , features = def
    , children = []
    }
</pre>
</div>

## type Step

A step constraint that implies use of an implicit [step](Pure.Stream/step) function. To use or produce the constrained value, the constraint must be satisfied. [stream](Pure.Stream/stream) is a function that will satisfy such a constraint on a [Streamer](Pure.Stream/data%20Streamer) type.

```haskell
type Step = (?step :: Int -> IO ())
```

## step

Step a lazy stream, forcing evaluation of the next suspended frame. This is often used in an [Observer](/packages/pure-intersection/0.8.0.0/Pure.Intersection/data%20Observer) to load more elements from the stream when the end of the stream is approached. See [stepper](Pure.Stream/stepper) for an example.

```haskell
step :: Step => IO ()
```

<div class="hide">
Step is a contextualized effect that requires constraint satisfaction for use. The type of [stream](Pure.Stream/stream) will guarantee satisfaction of the constraint for a supplied [Streamer](Pure.Stream/data%20Streamer). If more complex out-of-tree use is required, a communication channel will be necessary, after constraint satisfaction, to send the constraint-satisfied step function to an out-of-tree controller. This could be done with, for instance, the `children` of a [Streamer](Pure.Stream/data%20Streamer).
</div>

## steps

Step a lazy stream, forcing evaluation of the next *n* suspended frames. This is often used in an [Observer](/packages/pure-intersection/0.8.0.0/Pure.Intersection/data%20Observer) to load more elements from the stream when the end of the stream is approached. See [stepper](Pure.Stream/stepper) for an example of the slightly less generic, [step](Pure.Stream/step).

```haskell
steps :: Step => Int -> IO ()
```

<div class="hide">
Steps is a contextualized effect that requires constraint satisfaction for use. The type of [stream](Pure.Stream/stream) will guarantee satisfaction of the constraint for a supplied [Streamer](Pure.Stream/data%20Streamer). If more complex out-of-tree use is required, a communication channel will be necessary, after constraint satisfaction, to send the constraint-satisfied step function to an out-of-tree controller. This could be done with, for instance, the `children` of a [Streamer](Pure.Stream/data%20Streamer).
</div>

## stream

Convert a [Streamer](Pure.Stream/data%20Streamer) into a view. `stream` will satisfy a [Step](Pure.Stream/type%20Step) constraint.

```haskell
stream :: (Step => Streamer a) -> View
```

<div class="hide">
Note that the view produced from `stream` is nested.

```haskell
Div <| SetFeatures features |> 
  ( Div <||> consumer producer 
  : children
  )
```

If you need to target the nested &lt;div> with css, use a pseudo `:first-child` selector approach.

```haskell
myView = stream (myStream & Themed @MyStreamTheme)

data MyStreamTheme
instance Theme MyStreamTheme where
  theme c = void $ is c $ do
    apply $ do
      -- streamRootStyles
    is firstChild .> do
      -- streamStyles
```

This double-div avoids diffing the children unnecessarily.

If your use-case requires a custom shape, it should be relatively easy to create a custom implementation, similar to `stream`. 

<pre data-try>
import Pure
import Pure.Stream

fibs :: Stream IO Integer
fibs = unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 in more f (f1,f)

paragraph :: Integer -> View
paragraph n = P <||> [ txt n ]

main = inject body $ stream $ 
  frameStepper (100px) def
    { producer = chunksOf 10 (fmap paragraph fibs)
    , consumer = toList
    }
</pre>
</div>

## stepper

A default [Observer](/packages/pure-intersection/0.8.0.0/Pure.Intersection/data%20Observer) using an [IntersectionObserver](https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API) to trigger a [step](Pure.Stream/step).

```haskell
stepper :: Step => Observer
stepper = def 
        & Threshold [0] 
        & Action (\ts -> when (any intersecting ts) step)
```

<div class="hide">
<div class="info">
The default stepping offset is `0`. This is often not what you want to use, as it is often desirable to trigger the next suspended stream frame before the end of the stream is reached.
</div>

Note that this will not work correctly in this example because of the constraints implied by iframes. See [the spec](https://w3c.github.io/IntersectionObserver/#dom-intersectionobserver-rootmargin) for an explanation.

In the general case, this will work correctly, but in iframes, the [frameStepper](Pure.Stream/frameStepper) approach is required that hacks together a similar approach with relative+absolute positioning.

<pre data-try>
import Pure
import Pure.Stream

fibs :: Stream IO Integer
fibs = unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 in more f (f1,f)

paragraph :: Integer -> View
paragraph n = P <||> [ txt n ]

main = inject body $ 
  stream def
    { producer = chunksOf 10 (fmap paragraph fibs)
    , consumer = toList
    , children = [ stepper <| id ]
    }
</pre>
</div>

## frameStepper

A default [Observer](/packages/pure-intersection/0.8.0.0/Pure.Intersection/data%20Observer) using an [IntersectionObserver](https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API) along with relative+absolute positioning to trigger a [step](Pure.Stream/step). This approach, unlike [stepper](Pure.Stream/stepper) works within an &lt;iframe>.

```haskell
frameStepper :: Txt -> Streamer a -> Streamer a
```

<div class="hide">
<div class="info">
This approach is for vertically scrolling elements, as the stepping frame is positioned relative to the bottom of the containing element.
</div>

See [the spec](https://w3c.github.io/IntersectionObserver/#dom-intersectionobserver-rootmargin) for an explanation as to why this is necessary in an &lt;iframe>.

<pre data-try>
import Pure
import Pure.Stream

fibs :: Stream IO Integer
fibs = unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 in more f (f1,f)

paragraph :: Integer -> View
paragraph n = P <||> [ txt n ]

main = inject body $ 
  stream def
    { producer = chunksOf 10 (fmap paragraph fibs)
    , consumer = toList
    , children = [ stepper <| id ]
    }
</pre>
</div>

