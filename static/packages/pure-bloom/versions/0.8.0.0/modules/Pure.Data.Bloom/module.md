## data Bloom

The core mutable data structure holding a filter configuration and an `MVar` wrapping a mutable packed array of `Bool`. The constructor of this structure is opaque and hidden from export.

## encode

A custom encoder for [Bloom](Pure.Data.Bloom/data%20Bloom) to JSON `Value`.

```haskell
encode :: MonadIO m => Bloom -> m Value
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Data.Bloom as Bloom

main = do
  b <- new 0.01 500 

  let x = "ABCD" :: String

  add b x
  
  value <- encode b
  Just b' <- decode value

  exists <- test b' x

  inject body $ txt $ show exists
</pre>
</div>

## decode

A custom decoder for [Bloom](Pure.Data.Bloom/data%20Bloom) from JSON `Value`.

```haskell
decode :: MonadIO m => Value -> m (Maybe Bloom)
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Data.Bloom as Bloom

main = do
  b <- new 0.01 500 

  let x = "ABCD" :: String

  add b x

  value <- Bloom.encode b 
  Just b' <- Bloom.decode value

  exists <- test b' x

  inject body $ txt $ show exists
</pre>
</div>

## new

Construct a new Bloom filter from a desired false positive rate and an expected number of entries. This construction will choose the number of hashes and buckets to reach the desired false positive rate and size.

```haskell
new :: MonadIO m => Double -> Int -> m Bloom
```

<div class="hide">

<pre data-try>
import Pure
import Pure.Data.Bloom

main = do
  -- construct a new filter with a 1% 
  -- false positive rate for 500 entries
  b <- new 0.01 500 

  let x = "ABCD" :: String

  add b x
  exists <- test b x

  inject body $ txt $ show exists
</pre>
</div>

## add

Add a textual value to a Bloom filter.

```haskell
add :: (MonadIO m, ToTxt a) => Bloom -> a -> m ()
```

<div class="note">
The hashing algorithm used in this library uses the Kirsch-Mitzenmacher optimization with FNV-1a for improved performance.
</div>

<div class="hide">
<pre data-try>
import Pure
import Pure.Data.Bloom

main = do
  -- construct a new filter with a 1% 
  -- false positive rate for 500 entries
  b <- new 0.01 500 

  let v = "ABCD" :: String

  add b v
  exists <- test b v

  inject body $ txt $ show exists
</pre>
</div>

## test

Test a Bloom filter for the existence of a textual value.

```haskell
test :: (MonadIO m, ToTxt a) => Bloom -> a -> m Bool
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Data.Bloom

main = do
  -- construct a new filter with a 1% 
  -- false positive rate for 500 entries
  b <- new 0.01 500 

  let v = "ABCD" :: String

  add b v
  exists <- test b v

  inject body $ txt $ show exists
</pre>
</div>

## update

A combination of [add](Pure.Data.Bloom/add) and [test](Pure.Data.Bloom/test). Returns True if the filter was updated or False if the value already existed.

```haskell
update :: (MonadIO m, ToTxt a) => Bloom -> a -> m Bool
update bloom (toTxt -> val) = do
  b <- test bloom val
  unless b (add bloom val)
  pure (not b)
```

<div class="hide">
<pre data-try>
import Pure hiding (update)
import Pure.Data.Bloom

main = do
  -- construct a new filter with a 1% 
  -- false positive rate for 500 entries
  b <- new 0.01 500 

  let v = "ABCD" :: String

  add b v
  exists <- update b v

  inject body $ txt $ show exists
</pre>
</div>


## size

Approximate the count of elements currently in the filter.

```haskell
size :: MonadIO m => Bloom -> m Int
```

<div class="hide">
<pre data-try>
import Pure hiding (size)
import Pure.Data.Bloom

main = do
  let ks = [ [x,y] | let abcs = ['a'..'z'], x <- abcs, y <- abcs ]
  b <- new 0.01 2000
  for_ ks (add b)
  s <- size b
  inject body $ txt $ show (length ks,s)
</pre>
</div>

## union

Union two Bloom filters together.

The filters are required to have been constructed with the same options to [new](Pure.Data.Bloom/new), or `union` will return `Nothing`.

```haskell
union :: MonadIO m => Bloom -> Bloom -> m (Maybe Bloom)
```

<div class="hide">
<div class="note">
If an element does [test](Pure.Data.Bloom/test) `True` in one of the filters before the union, it will test `True` after the union. 
</div>

<pre data-try>
import Pure
import Pure.Data.Bloom

main = do
  l <- new 0.01 200
  r <- new 0.01 200
  let k1 = "abc" :: String
  let k2 = "bcd" :: String
  add l k1
  add r k2
  Just b <- union l r
  t1 <- test b k1
  t2 <- test b k2
  inject body $ txt $ show (t1,t2)
</pre>
</div>

## intersection

Intersect two Bloom filters.

The filters are required to have been constructed with the same options to [new](Pure.Data.Bloom/new), or `union` will return `Nothing`.

```haskell
intersection :: MonadIO m => Bloom -> Bloom -> m (Maybe Bloom)
```

<div class="hide">
<div class="note">
If an element does [test](Pure.Data.Bloom/test) `True` in both of the filters before the intersection, it should test `True` after the intersection. 
</div>

<pre data-try>
import Pure
import Pure.Data.Bloom

main = do
  l <- new 0.01 200
  r <- new 0.01 200
  let k1 = "abc" :: String
  let k2 = "bcd" :: String
  add l k1
  add r k1
  add r k2
  Just b <- intersection l r
  t1 <- test b k1
  t2 <- test b k2
  inject body $ txt $ show (t1,t2)
</pre>
</div>

