## data Stream

A delimited stream type.

```haskell
data Stream f a 
  = Nil 
  | Suspended (f (Stream f a)) 
  | Cons a (Stream f a)
```

<div class="warn">
Pattern matching on this type will prevent fusion.
</div>


## unfolds

Generate a *Stream* with an effectful and stateful anamorphism. Takes an initial state and a producing function that can be used with the convenience functions [more](Pure.Stream.Internal/more) and [done](Pure.Stream.Internal/done).

```haskell
unfolds :: Functor f 
        => state
        -> (state -> f (Maybe (element, state))) 
        -> Stream f element
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure
import Data.Functor.Identity
import Prelude hiding (take)
import Pure.Stream

fibs :: Stream Identity Integer
fibs = unfolds (0,1) $ \(f2,f1) -> 
  let f = f2 + f1 
  in more f (f1,f)

main = inject body . txt . show $ 
  runIdentity $ 
    toListM (take 10 fibs)
</pre>
</div>

## folds

Reduce a *Stream* with a generic `foldr`-style catamorphism. Note that, similar to `foldr`, the result is quite generic, allowing for `b` to be, for instance, a function of multiple parameters, making the fold stateful.

```haskell
folds :: Functor f 
      => b 
      -> (f b -> b) 
      -> (element -> b -> b) 
      -> Stream f element 
      -> b
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
`folds` pairs with the internal function `builds` and the `folds/builds` fusion rewrite rule to allow many streaming computations to avoid generating intermediate `Stream` structures.
</div>

## builds

A low-level utility for functional construction of streams that can help to enable folds/builds fusion. Most functions in this module use a combination of builds with folds to allow them to be fused away; when fusion fails or ends, in complex cases, builds will simply fill in Nil/Suspended/Cons.

```haskell
builds :: (forall b. b -> (f b -> b) -> (a -> b -> b) -> b) -> Stream f a
builds f = f Nil Suspended Cons
```

## more

A convenience function used with [unfolds](Pure.Stream.Internal/unfolds) to produce a stream element and update the state of a stream unfold. 

```haskell
more :: Applicative f 
     => element 
     -> state 
     -> f (Maybe (element,state))
```

<div class="hide">
<pre data-try>
import Pure hiding (count)
import Pure.Stream.Internal
import Data.Functor.Identity

count = unfolds 0 $ \i -> 
  if i == 10 
    then done 
    else more i (i + 1)

main = inject body . txt . show $ 
  runIdentity (toListM count)
</pre>
</div>

## done

A convenience function used with [unfolds](Pure.Stream.Internal/unfolds) to notify the unfold that the end of the stream has been reached. 

```haskell
done :: Applicative f => f (Maybe (element,state))
```

<div class="hide">
<pre data-try>
import Pure hiding (count)
import Pure.Stream.Internal
import Data.Functor.Identity

count = unfolds 0 $ \i -> 
  if i == 10 
    then done 
    else more i (i + 1)

main = inject body . txt . show $ 
  runIdentity (toListM count)
</pre>
</div>

## step

Evaluate the first suspended frame of a stream. This is the key function used in [Pure.Stream](Pure.Stream) to create lazy-loading streaming views. Every time a new segment of the stream is required, it is `step`ped.

```haskell
step :: Monad f => Stream f a -> f (Stream f a)
```

<div class="warn">
This function is fusion-breaking.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step,count)
import Pure.Stream.Internal
import Data.Functor.Identity

count = unfolds 0 $ \i -> 
  if i == 10 
    then done 
    else more i (i + 1)

main = inject body . txt . show .
  toList =<< step count
</pre>
</div>

## steps

[step](Pure.Stream.Internal/step) the first *n* suspended frames of a stream.

<div class="warn">
This function is fusion-breaking.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (steps,count)
import Pure.Stream.Internal
import Data.Functor.Identity

count = unfolds 0 $ \i -> 
  if i == 10 
    then done 
    else more i (i + 1)

main = inject body . txt . show . 
  toList =<< steps 3 count
</pre>
</div>

## force

Wrap a stream with a suspension that, when [stepped](Pure.Stream.Internal/step), will force a suspended frame to be evaluated.

```haskell
force :: Monad f => Int -> Stream f a -> Stream f a
```

Equivalent to

```haskell
force = suspended . step
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step,force)
import Pure.Stream.Internal

stream :: Stream IO Int
stream = s (s (s (c 1 (s (c 2 nil)))))
  where
    s = suspended . pure
    c = cons 

main = inject body . txt . show .
  toList =<< step (force 3 stream)
</pre>
</div>

## forceAll

Wrap a stream with a suspension that, when [stepped](Pure.Stream.Internal/step), will force all suspended frames to be evaluated.

```haskell
forceAll :: Monad f => Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
Compare this example with that of [force](Pure.Stream.Internal/force).

<pre data-try>
import Pure hiding (step,force)
import Pure.Stream.Internal

stream :: Stream IO Int
stream = s (s (s (c 1 (s (c 2 nil)))))
  where
    s = suspended . pure
    c = cons 

main = inject body . txt . show . 
  toList =<< step (forceAll stream)
</pre>
</div>

## stepSize

Make [step](Pure.Stream.Internal/step) always evaluate *n* suspended frames. 

```haskell
stepSize :: Monad f => Int -> Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
This is subtly different than [chunksOf](Pure.Stream.Internal/chunksOf) which always evaluates enough suspended frames to generate *n* elements. In many cases, however, these two will be functionally equivalent.

<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal

fibs = stepSize 10 $ unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 
  in more f (f1,f)

main = inject body . txt . show . 
  toList =<< step fibs
</pre>
</div>

## chunksOf

Make [step](Pure.Stream.Internal/step) always evaluate enough suspended frames to generate *n* elements.

```haskell
chunksOf :: Monad f => Int -> Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
This is subtly different than [stepSize](Pure.Stream.Internal/stepSize) which always evaluates *n* suspended frames. In many cases, however, these two will be functionally equivalent.

<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal

fibs = chunksOf 10 $ unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 
  in more f (f1,f)

main = inject body . txt . show . 
  toList =<< step fibs
</pre>
</div>

## toList

Convert the stream to a list, up to the first suspended frame or stream end.

```haskell
toList :: Functor f => Stream f a -> [a]
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal

fibs = unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 in more f (f1,f)

main = inject body . txt . show . 
  toList =<< step fibs
</pre>
</div>


## toListM

Convert the stream to a list, monadically. This will force the entire stream to be evaluated.

```haskell
toListM :: Monad f => Stream f a -> f [a]
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal as Stream

fibs :: Stream IO Integer
fibs = Stream.take 10 $ unfolds (0,1) $ \(f2,f1) ->
  let f = f2 + f1 in more f (f1,f)

main = inject body . txt . show =<<
  toListM fibs
</pre>
</div>

## fromList

Convert a list to a stream. No stream frames will be suspended.

```haskell
fromList :: [a] -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show $ 
  toList @Identity (fromList [1..10])
</pre>
</div>

## fromListM

Convert a list of actions into a stream. All frames will be suspended.

```haskell
fromListM :: Functor f => [f a] -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal as Stream

stream = fromListM [ pure x | x <- [ 1..10 ] ]

main = inject body . txt . show .
  toList =<< step stream
</pre>
</div>

## append

Append two streams.

```haskell
append :: Functor f => Stream f a -> Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show $ 
  toList @Identity (append [1,2,3] [4,5,6])
</pre>
</div>

## concat

Collapse a stream of streams of elements into a stream of elements. This can be necessary when generating a stream of elements at every step of an [unfolds](Pure.Stream.Internal/unfolds).

```haskell
concat :: Functor f => Stream f (Stream f a) -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show $
  toList @Identity (Stream.concat [[1,2,3],[4,5,6]])
</pre>
</div>

## repeat

Create an infinite stream of the given element. Each frame of the stream will be suspended.

```haskell
repeat :: Applicative f => a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal as Stream

stream :: Stream IO Int
stream = Stream.take 10 (Stream.repeat 1)

main = inject body . txt . show .
  toList =<< step stream
</pre>
</div>

## repeatM

Create an infinite stream of the given element. Each frame of the stream will not be suspended.

```haskell
repeatM :: Applicative f => f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (step)
import Pure.Stream.Internal as Stream

stream :: Stream IO Int
stream = Stream.take 10 (repeatM (pure 1))

main = inject body . txt . show .
  toList =<< step stream
</pre>
</div>

## cycle

Cycle a stream, infinitely. 

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream

stream :: Stream IO Int
stream = Stream.take 10 (Stream.cycle [1,2,3])

main = inject body . txt . show $
  toList stream
</pre>
</div>


## infinite

Create an infinite stream of the given element. Each frame of the stream will *not* be suspended.

```haskell
infinite :: Functor f => a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
import Pure
import Pure.Stream.Internal as Stream

stream :: Stream IO Int
stream = Stream.take 10 (Stream.infinite 1)

main = inject body . txt . show $
  toList stream
</pre>
</div>

## take

Take *n* elements from a stream, through suspended frames.

```haskell
take :: Functor f => Int -> Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream

stream :: Stream IO Int
stream = Stream.take 3 [1..10]

main = inject body . txt . show $
  toList stream
</pre>
</div>

## drop

Drop *n* elements from a stream, through suspended frames. The effects of suspended frames through which the *n* elements are found, will still be evaluated.

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream

stream :: Stream IO Int
stream = Stream.drop 3 [1..10]

main = inject body . txt . show $
  toList stream
</pre>
</div>

## null

Test if a stream is null. `True` is only returned if the stream is `Nil`. If there are any suspended frames or elements, the stream is considered non-null.

```haskell
null :: Functor f => Stream f a -> Bool
```

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show $ 
  Stream.null @Identity []
</pre>
</div>

## tail

Drop the first element from a stream. This function will work through suspended frames.

```haskell
tail :: Functor f => Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show $ 
  toList @Identity (Stream.tail [1..10])
</pre>
</div>

## reverse

Reverse a stream. This function will work through suspended frames.

```haskell
reverse :: Functor f => Stream f a -> Stream f a
```

<div class="info">
Subject to fusion, but does force the entire stream into memory.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show . toList @Identity $ 
  Stream.reverse [1..10] 
</pre>
</div>

## filter

Filter a stream, given a predicate. This function will work through suspended frames.

```haskell
filter :: Functor f => (a -> Bool) -> Stream f a -> Stream f a
```

<div class="info">
Subject to fusion.
</div>

<div class="hide">
<pre data-try>
{-# language OverloadedLists #-}
import Pure
import Pure.Stream.Internal as Stream
import Data.Functor.Identity

main = inject body . txt . show . toList @Identity $ 
  Stream.filter Prelude.even [1..10] 
</pre>
</div>

