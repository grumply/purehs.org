## data Variance

The internal state type to the [vary](Pure.Variance/vary) algorithm. It is produced as an intermediate result that can be amended, or inspected. This approach allows for the implementation of online and parallel variance algorithms.

`Variance` has instances of `Monoid` and `Semigroup` that enable a parallel divide-and-conquer approach to variance analysis by implementing a [parallel variant](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm).

The `Variance` constructor and accessors aren't exported, and instead smart accessors are exported to prevent mis-use when sample size was 0. The following smart accessors are available.

  * [count](Pure.Variance/Pure.Variance/count)
  * [mean](Pure.Variance/mean)
  * [minimum](Pure.Variance/minimum)
  * [maximum](Pure.Variance/maximum)
  * [stdDev](Pure.Variance/stdDev)
  * [variance](Pure.Variance/variance)

## vary

Integrate a single sample into a [Variance](Pure.Variance/data%20Variance) analysis. This is the core of [Welford's online algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm). This function extracts the values of interest via a selector function, which can be useful in partial applications: see `var` in the example below.

```haskell
vary :: Real b => (a -> b) -> a -> Variance -> Variance
```

<div class="hide">
See [varies](Pure.Variance/varies) for analyzing a sample.

<pre data-try>
import Pure
import Pure.Variance

var :: Real x => (x,y) -> Variance -> Variance
var = vary fst

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
  let v :: Variance
      v = foldr var mempty sample
  in inject body $ txt $ show (variance v)
</pre>
</div>

## varies

Determine the [Variance](Pure.Variance/data%20Variance) of a `Foldable` of `Real` values. 

```haskell
varies :: (Foldable f, Real b) => (a -> b) -> f a -> Variance
varies = foldl' (flip vary) mempty
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Variance

samples = 
  [ (3234.85,443.01) 
  , (3248.92,780   ) 
  , (3003.37,745.51) 
  , (2488.65,480.01) 
  , (2842.74,761.19) 
  , (3122.87,882.96) 
  ]

main = inject body $ txt $ show $
  variance (varies fst samples)
</pre>
</div>

## count

View the size of an analyzed sample.

```haskell
count :: Variance -> Int
```

## mean

View the sample mean, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
mean :: Variance -> Maybe Double
```

## minimum

View the sample minimum, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
minimum :: Variance -> Maybe Double
```

## maximum

View the sample maximum, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
maximum :: Variance -> Maybe Double
```

## variance

View the variance of a sample, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
variance :: Variance -> Maybe Double
variance = sampleVariance
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction). 
</div>

<pre data-try>
import Pure
import Pure.Variance

var :: Variance
var = varies id
  [ 443.01, 780, 745.51, 480.01, 761.19, 882.96 ]

main = inject body $ txt $ show (variance var)
</pre>
</div>

## sampleVariance

View the variance of a sample, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
sampleVariance :: Variance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction). 
</div>

<pre data-try>
import Pure
import Pure.Variance

var :: Variance
var = varies id
  [ 443.01, 780, 745.51, 480.01, 761.19, 882.96 ]

main = inject body $ txt $ show (sampleVariance var)
</pre>
</div>

## populationVariance

View the variance of a population, or `Nothing` if the population [count](Pure.Variance/count) is 0.

```haskell
populationVariance :: Variance -> Maybe Double
```

<div class="hide">
<div class="info">
Does not apply [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction). 
</div>

<pre data-try>
import Pure
import Pure.Variance

var :: Variance
var = varies id
  [ 443.01, 780, 745.51, 480.01, 761.19, 882.96 ]

main = inject body $ txt $ show (populationVariance var)
</pre>
</div>

## stdDev

View the standard deviation of a sample, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
stdDev :: Variance -> Maybe Double
stdDev = sampleStdDev
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Variance

var :: Variance
var = varies id
  [ 443.01, 780, 745.51, 480.01, 761.19, 882.96 ]

main = inject body $ txt $ show (stdDev var)
</pre>
</div>

## sampleStdDev

View the standard deviation of a sample, or `Nothing` if the sample [count](Pure.Variance/count) is 0.

```haskell
sampleStdDev :: Variance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Variance

var :: Variance
var = varies id
  [ 443.01, 780, 745.51, 480.01, 761.19, 882.96 ]

main = inject body $ txt $ show (sampleStdDev var)
</pre>
</div>

## populationStdDev

View the standard deviation of a population, or `Nothing` if the population [count](Pure.Variance/count) is 0.

```haskell
populationStdDev :: Variance -> Maybe Double
```

<div class="hide">
<div class="info">
Applies [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).
</div>

<pre data-try>
import Pure
import Pure.Variance

var :: Variance
var = varies id
  [ 443.01, 780, 745.51, 480.01, 761.19, 882.96 ]

main = inject body $ txt $ show (populationStdDev var)
</pre>
</div>

## data Variances

A structure for storing multiple tagged [Variance](Pure.Variance/data%20Variance) structures. Used for storing the analysis of data supporting the [Vary](Pure.Variance/class%20Vary) typeclass when using [varied](Pure.Variance/varied) and [variances](Pure.Variance/variances) for integrating a sample into a `Variances`.

```haskell
newtype Variances = Variances (HashMap String Variance)
```

See [lookupVariance](Pure.Variance/lookupVariance) for extracting results from a `Variances`.

## class Vary

A typeclass of a single function, `varied`, for extracting `Real` values of interest from a structure. If we know how to uniquely pull values from a structure, we can analyze a set of those structures.

Implementation of this class for a custom type is expected to be performed by deriving with `DeriveAnyClass`.

<div class="hide">
There are default instances for base `Real` types, as well as an instance for tuples, functorial wrappers, and vectors of `Real` values that are index-dependent. There are generic instances for products, sums, and records of those, making it easy to derive this class for types that implement `Generic`.

The major downside of this approach is that keys are stringly-typed.

```haskell
class Vary a where
  varied :: String -> a -> Variances -> Variances
```

<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Variance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Vary)

var :: Variances
var = variances
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = inject body $ txt $ show $ 
  ( fmap variance $ lookupVariance "height" var
  , fmap variance $ lookupVariance "grade"  var
  , fmap variance $ lookupVariance "age"    var
  )
</pre>
</div>


## lookupVariance

Retrieve a [Variance](Pure.Variance/data%20Variance) at a given index in a [Variances](Pure.Variance/data%20Variances) map.

```haskell
lookupVariance :: String -> Variances -> Maybe Variance
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Variance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Vary)

var :: Variances
var = variances
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = inject body $ txt $ show $ 
  ( fmap variance $ lookupVariance "height" var
  , fmap variance $ lookupVariance "grade" var
  , fmap variance $ lookupVariance "age"    var
  )
</pre>
</div>

## variances

Analyze a sample of foldable [Varying](Pure.Variance/class%20Vary) values into a [Variances](Pure.Variance/data%20/Variances) result.

```haskell
variances :: (Foldable f, Vary a) => f a -> Variances
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.Variance
import GHC.Generics

data Student = Student 
  { age    :: Double
  , height :: Double 
  , grade  :: Double
  } deriving (Generic,Vary)

var :: Variances
var = variances
  [ Student 5 100 80
  , Student 5 105 95
  , Student 7 120 55
  , Student 7 122 89
  , Student 9 135 75
  , Student 9 130 98
  ]

main = inject body $ txt $ show $ 
  ( fmap variance $ lookupVariance "height" var
  , fmap variance $ lookupVariance "grade" var
  , fmap variance $ lookupVariance "age"    var
  )
</pre>
</div>
