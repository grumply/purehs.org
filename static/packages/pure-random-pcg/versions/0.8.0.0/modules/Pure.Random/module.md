## newtype Generator

The `Generator` environment supports a basic monadic interface as well as a bevy of support interfaces.

```haskell
newtype Generator a = Generator { generate :: Seed -> (Seed,a) }
```

<div class="hide">

A functor instance allows transformations of generators.

<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  -- uniform for Double produces in the range [0,1]
  let (_,x :: Double) = generate (fmap (+1) uniform) s
  inject body $ txt $ show x
</pre>

An applicative instance allows for independent composition of generators.

<pre data-try>
import Pure
import Pure.Random

data C = Double :+ Double deriving Show

randomC :: Generator C
randomC = pure (:+) <*> uniform <*> uniform

main = do
  s <- newSeed
  let (_,c) = generate randomC s
  inject body $ txt $ show c
</pre>

A monadic instance allows for dependent generators.

<pre data-try>
-- pedagogical; not performant
import Pure
import Pure.Random

data Rank = Rank Int deriving (Show,Eq,Ord)
data Suit = Suit Int deriving (Show,Eq,Ord)

data Card = Card Suit Rank deriving (Show,Eq,Ord)

rank = Rank <$> uniformR 1 13
suit = Suit <$> uniformR 1 4
card = Card <$> suit <*> rank

cards :: [Card]
cards = 
  [ Card s r
  | r0 <- [1..13]
  , s0 <- [1..4]
  , let r = Rank r0
  , let s = Suit s0
  ]

-- stateful generation
deal :: Int -> Generator [Card]
deal = go cards
  where
    go _ 0 = pure []
    go deck n = do
      c <- card
      if c `elem` deck
      then do
        let remaining = filter (/= c) deck
        cs <- go remaining (n - 1)
        pure (c:cs)
      else
        go deck n
 
main = do
  s  <- newSeed
  let (_,cs) = generate (deal 5) s
  inject body $ txt $ show cs
</pre>
</div>

## type Seed

The `Seed` type that fuels the [Generators](Pure.Random/newtype%20Generator).

Seeds can be constructed randomly in `IO` with [newSeed](Pure.Random/newSeed), constructed deterministically with [initialSeed](Pure.Random/initialSeed), or forked with [independentSeed](Pure.Random/independentSeed).

Running a generator will consume a seed and produce a new seed. Seed reuse is inadvisable but cannot be prevented with this library formulation.

## generate

Run a [Generator](Pure.Random/newtype%20Generator), producing a random value and a new [Seed](Pure.Random/type%20Seed).

```haskell
generate :: Generator a -> Seed -> (Seed,a)
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  inject body $ txt $ show $
    generate @Double uniform s
</pre>
</div>


## newSeed

Generate a new [Seed](Pure.Random/type%20Seed) in `IO`.

```haskell
newSeed :: IO Seed
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  inject body $ txt $ show s
</pre>
</div>

## initialSeed

Deterministically construct a [Seed](Pure.Random/type%20Seed). This is especially useful for deterministic reproduction of random processes.

```haskell
initialSeed :: Int -> Seed
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  let s = initialSeed 42
  inject body $ txt $ show $
    generate @Int uniform s
</pre>
</div>

## independentSeed

Generate an independent seed.

```haskell
independentSeed :: Generator Seed
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  inject body $ txt $ show 
    (s,generate independentSeed s)
</pre>
</div>


## class Variate

The typeclass of uniform variate generation. Supports most basic numeric types.

<div class="note">
All integral types produce in the range [inclusive,inclusive], and fractional types produce in the range (exclusive,inclusive].
</div>

```haskell
class Variate a where
  uniform :: Generator a
  uniformR :: a -> a -> Generator a
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  let (_,u) = generate (uniformR @Int 1 10) s
  inject body $ txt $ show u
</pre>
</div>

## bool

A fair `Bool` generator.

```haskell
bool :: Generator Bool
bool = oneIn 2
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  let (_,b) = generate bool s
  inject body $ txt $ show b
</pre>
</div>

## oneIn

A trial generator for a given odds.

```haskell
oneIn :: Int -> Generator Bool
oneIn n = pure (== 1) <*> uniformR 1 n
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  let (_,b) = generate (oneIn 5) s
  inject body $ txt $ show b
</pre>
</div>

## sample

Sample evenly from a foldable.

```haskell
sample :: Foldable f => f a -> Generator (Maybe a)
```

<div class="hide">
<div class="note">
Returns `Nothing` in the case that your foldable is empty.
</div>

<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  inject body $ txt $ show $ 
    generate (sample [1..10]) s
</pre>
</div>

## sampleVector

Sample evenly from a vector. This is more performant than [sample](Pure.Random/sample), if you already have a vector.

```haskell
sampleVector :: Vector v a => v a -> Generator (Maybe a)
```

<div class="hide">
<div class="note">
Returns `Nothing` in the case that your vector is zero-length.
</div>

<pre data-try>
import Pure
import Pure.Random

import Data.Vector

main = do
  s <- newSeed
  inject body $ txt $ show $ 
    generate (sampleVector (fromList [1..10])) s
</pre>
</div>

## shuffle

Shuffle a list, consuming the seed. 

```haskell
shuffle :: [a] -> Seed -> [a]
```

<div class="hide">
Implemented as a basic [Fisher-Yates shuffle](https://en.wikipedia.org/wiki/Fisher–Yates_shuffle).

<div class="note">
If you need pure access to a new seed, use [independentSeed](Pure.Random/independentSeed) to fork a seed for the shuffle.
</div>

<pre data-try>
import Pure
import Pure.Random

main = do
  s <- newSeed
  inject body $ txt $ show $ 
    shuffle [1..10] s
</pre>
</div>

## shuffleVector

Shuffle a vector, consuming the seed. This is more performant than [shuffle](Pure.Random/shuffle), if you already have a vector.

```haskell
shuffleVector :: Vector v a => v a -> Seed -> v a
```

<div class="hide">
Implemented as a basic [Fisher-Yates shuffle](https://en.wikipedia.org/wiki/Fisher–Yates_shuffle).

<div class="note">
If you need pure access to a new seed, use [independentSeed](Pure.Random/independentSeed) to fork a seed for the shuffle.
</div>

<pre data-try>
import Pure
import Pure.Random

import Data.Vector (fromList)

main = do
  s <- newSeed
  inject body $ txt $ show $ 
    shuffleVector (fromList [1..10]) s
</pre>
</div>

## shuffleMVector

Shuffle a vector, mutably, consuming the seed. This is more performant than [shuffle](Pure.Random/shuffle) or [shuffleVector](Pure.Random/shuffleVector), if you already have a mutable vector.

```haskell
shuffleMVector :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Seed -> m ()
```

<div class="hide">
Implemented as a basic in-place [Fisher-Yates shuffle](https://en.wikipedia.org/wiki/Fisher–Yates_shuffle).

<div class="note">
If you need pure access to a new seed, use [independentSeed](Pure.Random/independentSeed) to fork a seed for the shuffle.
</div>

<pre data-try>
import Pure hiding (modify)
import Pure.Random

import Data.Vector (fromList,modify)

main = do
  s <- newSeed
  let v = modify (flip shuffleMVector s) (fromList [1..10])
  inject body $ txt $ show v
</pre>
</div>

## choose

Select a random bounded enumerable.

```haskell
choose :: forall a. (Bounded a,Enum a) => Generator a
choose = pure toEnum <*> uniformR 0 (fromEnum (maxBound :: a))
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

data Cardinal = North | East | South | West
  deriving (Bounded,Enum,Show)

main = do
  s <- newSeed
  inject body $ txt $ show $ 
    generate (choose @Cardinal) s
</pre>
</div>

## vector

Generate a random vector of a given length from a given generator.

```haskell
vector :: Vector v a => Int -> Generator a -> Generator (v a)
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Random

import Data.Vector (Vector)

main = do
  s <- newSeed
  inject body $ txt $ show $ 
    generate (vector @Vector 10 (uniformR @Int 1 10)) s
</pre>
</div>