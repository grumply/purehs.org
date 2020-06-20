## data Covariance

The internal state type to the [covary](Pure.Covariance/covary) algorithm. It is produced as an intermediate result that can be amended, or inspected. This approach allows for the implementation of online and parallel covariance algorithms.

`Covariance` has instances of `Monoid` and `Semigroup` that enable a divide-and-conquer approach to covariance analysis by implementing a [numerically stable parallel variant](https://dl.acm.org/citation.cfm?doid=3221269.3223036) of the covariance algorithm.

The `Covariance` constructor and accessors aren't exported, and instead smart accessors are exported to prevent mis-use in the case that the sample size is 0. The following smart accessors are available.

  * [count](Pure.Covariance/count)
  * [meanX](Pure.Covariance/meanX)
  * [meanY](Pure.Covariance/meanY)
  * [stdDevX](Pure.Covariance/stdDevX)
  * [stdDevY](Pure.Covariance/stdDevY)
  * [varianceX](Pure.Covariance/varianceX)
  * [varianceY](Pure.Covariance/varianceY)
  * [covariance](Pure.Covariance/covariance)
  * [correlation](Pure.Covariance/correlation)

## covary

Integrate a single sample into a [Covariance](Pure.Covariance/data%20Covariance) analysis. This is the core of the [online covariance algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online). This function extracts the values of interest via selector functions, which can be useful in partial applications: see `cov` in the example below.

```haskell
covary :: (Real x, Real y) 
       => (a -> x) 
       -> (a -> y) 
       -> a 
       -> Covariance 
       -> Covariance
```

<div class="hide">
See [covaries](Pure.Covariance/covaries) for analyzing a sample.

<pre data-try>
import Pure
import Pure.Covariance

cov :: (Real x, Real y) => (x,y) -> Covariance -> Covariance
cov = covary fst snd

-- (S&P 500,$TSLA) at one-month intervals
sample :: [(Double,Double)]
sample = 
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

main = 
  let c :: Covariance
      c = foldr cov mempty sample
  in inject body $ txt $ show (covariance c,correlation c)

-- Given this small sample, the positive covariance tells 
-- us that they tend to move in the same direction, but 
-- the low correlation tells us that they're not especially 
-- correlated.
</pre>
</div>

## covaries

Determine the [Covariane](Pure.Covariance/data%20Covariance) of a `Foldable` of products of `Real` values, given functional extractors for those real values.  

```haskell
covaries :: (Foldable f, Real x, Real y) 
         => (a -> x) 
         -> (a -> y) 
         -> f a 
         -> Covariance
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Covariance

-- (S&P 500,$TSLA) at one-month intervals
sample :: [(Double,Double)]
sample = 
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

main = 
  let c :: Covariance
      c = covaries fst snd sample
  in inject body $ txt $ show 
      (covariance c,correlation c)

-- Given this small sample, the positive covariance tells 
-- us that they tend to move in the same direction, but 
-- the low correlation tells us that they're not especially 
-- correlated.
</pre>
</div>

## count

View the size of an analyzed sample.

```haskell
count :: Covariance -> Int
```

## meanX

View the mean for the first set of values, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
meanX :: Covariance -> Maybe Double
```

## meanY

View the mean for the second set of values, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
meanY :: Covariance -> Maybe Double
```

## minimumX

View the minimum for the first set of values, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
minimumX :: Variance -> Maybe Double
```

## maximumX

View the maximum for the first set of values, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
maximumX :: Variance -> Maybe Double
```

## minimumY

View the minimum for the second set of values, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
minimumY :: Variance -> Maybe Double
```

## maximumY

View the maximum for the second set of values, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
maximumY :: Variance -> Maybe Double
```

## covariance

View the covariance of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
covariance :: Covariance -> Maybe Double
covariance = sampleCovariance
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction). 
</div>

<pre data-try>
import Pure
import Pure.Covariance

-- (S&P 500,$TSLA) at one-month intervals
cov :: Covariance
cov = covaries fst snd
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

main = inject body $ txt $ show (covariance cov)

-- Given this small sample, the positive covariance tells 
-- us that they tend to move in the same direction.
</pre>
</div>

## sampleCovariance

View the covariance of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
sampleCovariance :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

-- (S&P 500,$TSLA) at one-month intervals
cov :: Covariance
cov = covaries fst snd
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

main = inject body $ txt $ show (sampleCovariance cov)

-- Given this small sample, the positive covariance tells 
-- us that they tend to move in the same direction.
</pre>
</div>

## populationCovariance

View the covariance of a population, or `Nothing` if the population [count](Pure.Covariance/count) is 0.

```haskell
populationCovariance :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (populationCovariance cov)
</pre>
</div>

## varianceX

View the variance of the first set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
varianceX :: Covariance -> Maybe Double
varianceX = sampleVarianceX
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (varianceX cov)
</pre>
</div>

## sampleVarianceX

View the variance of the first set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
sampleVarianceX :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (sampleVarianceX cov)
</pre>
</div>

## populationVarianceX

View the variance of the first set of values of a population, or `Nothing` if the population [count](Pure.Covariance/count) is 0.

```haskell
populationVarianceX :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (populationVarianceX cov)
</pre>
</div>

## varianceY

View the variance of the second set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
varianceY :: Covariance -> Maybe Double
varianceY = sampleVarianceY
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (varianceY cov)
</pre>
</div>

## sampleVarianceY

View the variance of the second set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
sampleVarianceY :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (sampleVarianceY cov)
</pre>
</div>

## populationVarianceY

View the variance of the second set of values of a population, or `Nothing` if the population [count](Pure.Covariance/count) is 0.

```haskell
populationVarianceY :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (populationVarianceY cov)
</pre>
</div>

## stdDevX

View the standard deviation of the first set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
stdDevX :: Covariance -> Maybe Double
stdDevX = sampleStdDevX
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (stdDevX cov)
</pre>
</div>

## sampleStdDevX

View the standard deviation of the first set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
sampleStdDevX :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (sampleStdDevX cov)
</pre>
</div>

## populationStdDevX

View the standard deviation of the first set of values of a population, or `Nothing` if the population [count](Pure.Covariance/count) is 0.

```haskell
populationStdDevX :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (populationStdDevX cov)
</pre>
</div>

## stdDevY

View the standard deviation of the second set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
stdDevY :: Covariance -> Maybe Double
stdDevY = sampleStdDevY
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (stdDevY cov)
</pre>
</div>

## sampleStdDevY

View the standard deviation of the second set of values of a sample, or `Nothing` if the sample [count](Pure.Covariance/count) is 0.

```haskell
sampleStdDevY :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (sampleStdDevY cov)
</pre>
</div>

## populationStdDevY

View the standard deviation of the second set of values of a population, or `Nothing` if the population [count](Pure.Covariance/count) is 0.

```haskell
populationStdDevY :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

cov :: Covariance
cov = covaries fst snd [ (x,x * 2) | x <- [1..10] ]

main = inject body $ txt $ show (populationStdDevY cov)
</pre>
</div>

## correlation

View [Pearson's Correlation Coefficient](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) of a sample. The result is a value in the range `[-1,1]` where `-1` is total inverse linear correlation, `0` means uncorrelated, and `1` is total positive linear correlation.

```haskell
correlation :: Covariance -> Maybe Double
correlation = sampleCorrelation
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

-- Real-life sample of S&P500 and $TSLA sampled at 
-- one-month intervals. These should be somewhat 
-- correlated.
cov1 :: Covariance
cov1 = covaries fst snd
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

-- This should be perfectly positively linearly correlated.
cov2 :: Covariance
cov2 = covaries fst snd 
  [ (x,x + 1) | x <- [1..100] ]

-- These should be perfectly inversely linearly correlated.
cov3 :: Covariance
cov3 = covaries fst snd 
  [ (x,negate x) | x <- [1..100] ]

-- These should be largely uncorrelated.
cov4 :: Covariance
cov4 = covaries fst snd 
  [ (1,7),(2,3),(3,0),(4,10),(5,2)]

main = inject body $ txt $ show 
  (correlation cov1
  ,correlation cov2
  ,correlation cov3
  ,correlation cov4
  )
</pre>
</div>

## sampleCorrelation

View [Pearson's Correlation Coefficient](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) of a sample. The result is a value in the range `[-1,1]` where `-1` is inverse linear correlation, `0` means uncorrelated, and `1` is positive linear correlation.

```haskell
sampleCorrelation :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

-- Real-life sample of S&P500 and $TSLA sampled at 
-- one-month intervals. These should be somewhat 
-- correlated.
cov1 :: Covariance
cov1 = covaries fst snd
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

-- This should be perfectly positively linearly correlated.
cov2 :: Covariance
cov2 = covaries fst snd 
  [ (x,x + 1) | x <- [1..100] ]

-- These should be perfectly inversely linearly correlated.
cov3 :: Covariance
cov3 = covaries fst snd 
  [ (x,negate x) | x <- [1..100] ]

-- These should be largely uncorrelated.
cov4 :: Covariance
cov4 = covaries fst snd 
  [ (1,7),(2,3),(3,0),(4,10),(5,2)]

main = inject body $ txt $ show 
  (sampleCorrelation cov1
  ,sampleCorrelation cov2
  ,sampleCorrelation cov3
  ,sampleCorrelation cov4
  )
</pre>
</div>

## populationCorrelation

View [Pearson's Correlation Coefficient](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) of a population. The result is a value in the range `[-1,1]` where `-1` is total inverse linear correlation, `0` means uncorrelated, and `1` is total positive linear correlation.

```haskell
populationCorrelation :: Covariance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Covariance

-- Real-life sample of S&P500 and $TSLA sampled at 
-- one-month intervals. These should be somewhat 
-- correlated.
cov1 :: Covariance
cov1 = covaries fst snd
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

-- This should be perfectly positively linearly correlated.
cov2 :: Covariance
cov2 = covaries fst snd
  [ (x,x + 1) | x <- [1..100] ]

-- These should be perfectly inversely linearly correlated.
cov3 :: Covariance
cov3 = covaries fst snd
  [ (x,negate x) | x <- [1..100] ]

-- These should be largely uncorrelated.
cov4 :: Covariance
cov4 = covaries fst snd
  [ (1,7),(2,3),(3,0),(4,10),(5,2)]

main = inject body $ txt $ show 
  (populationCorrelation cov1
  ,populationCorrelation cov2
  ,populationCorrelation cov3
  ,populationCorrelation cov4
  )
</pre>
</div>

## data Covariances

A structure for storing multiple [Covariance](Pure.Covariance/data%20Covariance) structures. Used for storing the analysis of data supporting the [Covary](Pure.Covariance/class%20Covary) typeclass when using [covaried](Pure.Covariance/covaried) and [covariances](Pure.Covariance/covariances) for integrating a sample into a `Covariances`.

```haskell
newtype Covariances = Covariances (HashMap (String,String) Covariance)
```

See [lookupCovariance](Pure.Covariance/lookupCovariance) for extracting results from a `Covariances`.

## class Covary

A typeclass of a single function, `extract`, for extracting `Real` values of interest from a structure. If we know how to uniquely pull values from a structure we can analyze a set of those structures.

Implementation of this class for a custom type is expected to be performed by deriving with `DeriveAnyClass`.

<div class="hide">
There are default instances for tuples, functorial wrappers, and vectors of `Real` values that are index-dependent. There are generic instances for products, sums, and records of those, making it easy to derive this class for types that implement `Generic`.

The major downside of this approach is that keys are stringly-typed.

```haskell
class Covary a where
  extract :: String -> a -> [(String,Double)] -> [(String,Double)]
```

<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Covariance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Covary)

cov :: Covariances
cov = covariances
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = inject body $ txt $ show $ 
  ( fmap correlation $ lookupCovariance "height" "age"   cov
  , fmap correlation $ lookupCovariance "height" "grade" cov
  , fmap correlation $ lookupCovariance "age"    "grade" cov
  )
</pre>
</div>

## lookupCovariance

Retrieve a [Covariance](Pure.Covariance/data%20Covariance) at a given index pair in a [Covariances](Pure.Covariance/data%20Covariances) map. Order of the indices is not important.

```haskell
lookupCovariance :: String -> String -> Covariances -> Maybe Covariance
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Covariance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Covary)

cov :: Covariances
cov = covariances
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = inject body $ txt $ show $ 
  ( fmap correlation $ lookupCovariance "height" "age"   cov
  , fmap correlation $ lookupCovariance "height" "grade" cov
  , fmap correlation $ lookupCovariance "age"    "grade" cov
  )
</pre>
</div>

## covaried

Integrate a single [Covarying](Pure.Covariance/class%20Covary) sample into a [Covariances](Pure.Covariance/data%20Covariances) structure.

```haskell
covaried :: Covary a => a -> Covariances -> Covariances
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Covariance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Covary)

cov :: (Foldable f, Covary a) => f a -> Covariances
cov = foldr covaried mempty

students :: [Student]
students =
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = do
  let c = cov students
  inject body $ txt $ show $ 
    ( fmap correlation $ lookupCovariance "height" "age"   c
    , fmap correlation $ lookupCovariance "height" "grade" c
    , fmap correlation $ lookupCovariance "age"    "grade" c
    )
</pre>
</div>

## covariances

Analayze a sample of foldable [Covarying](Pure.Covariance/class%20Covary) values into a [Covariances](Pure.Covariance/data%20Covariances) result.

```haskell
covariances :: (Foldable f, Covary a) => f a -> Covariances
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Covariance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Covary)

cov :: Covariances
cov = covariances
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = inject body $ txt $ show $ 
  ( fmap correlation $ lookupCovariance "height" "age"   cov
  , fmap correlation $ lookupCovariance "height" "grade" cov
  , fmap correlation $ lookupCovariance "age"    "grade" cov
  )
</pre>
</div>
