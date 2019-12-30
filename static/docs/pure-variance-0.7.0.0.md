# pure-variance

This package implements incremental and incremental parallel algorithms for [variance](https://en.wikipedia.org/wiki/Variance) and [covariance](https://en.wikipedia.org/wiki/Covariance) as well as generic machinery for deriving analyses for arbitrary `Generic` structures containing varying and covarying values.

> `pure-variance` uses [Welford's online variance algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm) with the `Semigroup` and `Monoid` instances for [Variance](#variance) implementing the [parallel variant](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm). The [similar approach for covariance](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online) is used with the `Semigroup` and `Monoid` instances for [Covariance](#covariance) implementing the [parallel variant](https://dl.acm.org/citation.cfm?doid=3221269.3223036).

`pure-variance` has two approaches to population analysis; one may manually, and type-safely, analyze extraction functions or generically, and *stringly*-typed, for arbitrary `Generic` types containing varying or co-varying values.

## Pure.Variance

`Pure.Variance` exports utilites for two approaches to variance determination, the [functional approach](/doc/pure-variance/0.7.0.0/Pure.Variance/varies) and the [generic, class-based approach](/doc/pure-variance/0.7.0.0/Pure.Variance/variances).

### data Variance

The `Variance` data type stores the result of analyzing some aggregation varying of values.

> The `Variance` constructor isn't exported, and instead smart accessors are exported to prevent mis-use when sample size was 0. See [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count), [mean](/doc/pure-variance/0.7.0.0/Pure.Variance/mean), [minimum_](/doc/pure-variance/0.7.0.0/Pure.Variance/minimum_), [maximum_](/doc/pure-variance/0.7.0.0/Pure.Variance/maximum_), [stdDev](/doc/pure-variance/0.7.0.0/Pure.Variance/stdDev), [variance](/doc/pure-variance/0.7.0.0/Pure.Variance/variance).

```haskell
data Variance = Variance
  { vCount    :: Double
  , vMean     :: Double
  , vMean2    :: Double
  , vMinimum_ :: Double
  , vMaximum_ :: Double
  }
```

There are `Monoid` and `Semigroup` instances for `Variance` that allow for divide-and-conquer parallel variance determination.

### count

`count` returns the size of the analyzed sample.

```haskell
count :: Variance -> Int
```

### mean

`mean` is a smart accessor that returns the sample mean, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```hasekll
mean :: Variance -> Maybe Double
```

### minimum_

`minimum_` is a smart accessor that returns the smallest element seen in a sample, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```haskell
minimum_ :: Variance -> Maybe Double
```

### maximum_

`maximum_` is a smart accessor that returns the largest element seen in the a sample, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```haskell
maximum_ :: Variance -> Maybe Double
```

### variance

`variance` is a synonym for [sampleVariance](/doc/pure-variance/0.7.0.0/Pure.Variance/sampleVariance), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleVariance

`sampleVariance` returns the variance of a sample, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```haskell
sampleVariance :: Variance -> Maybe Double
```

### populationVariance

`populationVariance` returns the variance of a population, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```haskell
populationVariance :: Variance -> Maybe Double
```

### stdDev

`stdDev` is a synonym for [sampleStdDev](/doc/pure-variance/0.7.0.0/Pure.Variance/sampleStdDev), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).


### sampleStdDev

`sampleStdDev` returns the standard deviation of a sample variance, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```haskell
sampleStdDev :: Variance -> Maybe Double
```

### populationStdDev

`populationStdDev` return the standard deviation of a population's variance, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Variance/count) is 0.

```haskell
populationStdDev :: Variance -> Maybe Double
```

### vary

`vary` integrates a `Real` value into an existing `Variance`. This is the core of [Welford's online algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm). This method takes a selector function to match the `covary` interface.

```haskell
vary :: Real b => (a -> b) -> a -> Variance -> Variance
```

See [varies](/doc/pure-variance/0.7.0.0/Pure.Variance/varies) for analyzing a sample.

### varies

`varies` determines the `Variance` of a `Foldable` of `Real` values.

```haskell
varies :: (Foldable f, Real a) => f a -> Variance
varies = foldl' (flip vary) mempty
```

### data Varied

The `Varied` type is a newtype around a mapping from `String` to `Variance` for storing the results of a sample analysis of `Vary`ing structures of values.

```haskell
newtype Varied = Varied (HashMap String Variance)
```

See [lookupVariance](/doc/pure-variance/0.7.0.0/Pure.Variance/lookupVariance) for extracting results from a `Varied`.

### class Vary

The `Vary` class defines the method for extracting values of interest from a structure.

There are default instances for base `Real` types, as well as an instance for tuples, functorial wrappers, and vectors of `Real` values that are index-dependent. There are generic instances for products, sums, and records of those.

> If you know of some magic to derive the analysis of structures while maintaining type safety and performance without GHC preprocessors or higher-kinded data, I would love to see it!

The `Vary` function, `varied`, is designed/shaped to be a step in a fold for performance reasons.

```haskell
class Vary a where
  varied :: String -> a -> Varied -> Varied
```

### lookupVariance

`lookupVariance` attempts to retrieve the [Variance](/doc/pure-variance/0.7.0.0/Pure.Variance/data-variance) at a given index in a [Varied](/doc/pure-variance/0.7.0.0/Pure.Variance/data-varied) map.

```haskell
lookupVariance :: String -> Varied -> Maybe Variance
```

### variances

`variances` integrates a foldable of [Vary](/doc/pure-variance/0.7.0.0/Pure.Variance/class-vary) values into a flat map of `Variance`s.

```haskell
variances :: (Foldable f, Vary a) => f a -> Varied
```

## Pure.Covariance

`Pure.Covariance` exports utilites for two approaches to covariance determination, the [functional approach](/doc/pure-variance/0.7.0.0/Pure.Covariance/covaries) and the [generic, class-based approach](/doc/pure-variance/0.7.0.0/Pure.Covariance/covariances).

### data Covariance

The `Covariance` data type stores the result of analyzing some aggregation of co-varying values.

> The `Covariance` constructor isn't exported, and instead smart accessors are exported to prevent mis-use when sample size was 0. See [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count), [meanx](/doc/pure-variance/0.7.0.0/Pure.Covariance/meanx), [meany](/doc/pure-variance/0.7.0.0/Pure.Covariance/meany), [stdDev_x](/doc/pure-variance/0.7.0.0/Pure.Covariance/stdDev_x), [stdDev_y](/doc/pure-variance/0.7.0.0/Pure.Covariance/stdDev_y), [variance_x](/doc/pure-variance/0.7.0.0/Pure.Covariance/variance_x), [variance_y](/doc/pure-variance/0.7.0.0/Pure.Covariance/variance_y).

```haskell
data Covariance = Covariance
  { cCount    :: Double
  , cMeanx    :: Double
  , cMeany    :: Double
  , cMeanx2   :: Double
  , cMeany2   :: Double
  , cC        :: Double
  }
```

There are `Monoid` and `Semigroup` instances for `Covariance` that allow for divide-and-conquer parallel covariance determination.

### covary

`covary` integrates two `Real` values into an existing `Covariance`. This is the core of the [online covariance algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online). This function takes two selector functions for extracting the values from an aggregate.

```haskell
covary :: (Real x, Real y) => (a -> x) -> (a -> y) -> a -> Variance -> Variance
```

See [covaries](/doc/pure-variance/0.7.0.0/Pure.Covariance/covaries) for analyzing a sample.

### covaries

`covaries` determines the `Covariance` of a `Foldable` of products of `Real` values.

```haskell
covaries :: (Foldable f, Real x, Real y) => (a -> x) -> (a -> y) -> f a -> Variance
covaries f g = foldl' (flip (covary f g)) mempty
```

### count

`count` returns the size of the analyzed sample.

```haskell
count :: Covariance -> Int
```

### meanx

`meanx` is a smart accessor that returns the sample mean for `x`, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
meanx :: Covariance -> Maybe Double
```

### meany

`meany` is a smart accessor that returns the sample mean for `y`, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
meany :: Covariance -> Maybe Double
```

### covariance

`covariance` is a synonym for [sampleCovariance](/doc/pure-variance/0.7.0.0/Pure.Covariance/sampleCovariance), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleCovariance

`sampleCovariance` returns the covariance of a sample, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
sampleCovariance :: Covariance -> Maybe Double
```

### populationCovariance

`populationCovariance` returns the covariance of a population, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
populationCovariance :: Covariance -> Maybe Double
```

### variance_x

`variance_x` is a synonym for [sampleVariance_x](/doc/pure-variance/0.7.0.0/Pure.Covariance/sampleVariance_x), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleVariance_x

`sampleVariance_x` returns the variance of `x` for a sample, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
sampleVariance_x :: Covariance -> Maybe Double
```

### populationVariance_x

`populationVariance_x` returns the variance of `x` for a population, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
populationVariance_x :: Covariance -> Maybe Double
```

### variance_y

`variance_y` is a synonym for [sampleVariance_y](/doc/pure-variance/0.7.0.0/Pure.Covariance/sampleVariance_y), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleVariance_y

`sampleVariance_y` returns the variance of `y` for a sample, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
sampleVariance_y :: Covariance -> Maybe Double
```

### populationVariance_y

`populationVariance_y` returns the variance of `y` for a population, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
populationVariance_y :: Covariance -> Maybe Double
```

### stdDev_x

`stdDev_x` is a synonym for [sampleStdDev_x](/doc/pure-variance/0.7.0.0/Pure.Covariance/sampleStdDev_x), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleStdDev_x

`sampleStdDev_x` returns the standard deviation of `x` for a sample variance, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
sampleStdDev_x :: Covariance -> Maybe Double
```

### populationStdDev_x

`populationStdDev_x` return the standard deviation of `x` for a population's variance, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
populationStdDev_x :: Covariance -> Maybe Double
```

### stdDev_y

`stdDev_y` is a synonym for [sampleStdDev_y](/doc/pure-variance/0.7.0.0/Pure.Covariance/sampleStdDev_y), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleStdDev_y

`sampleStdDev_y` returns the standard deviation of `y` for a sample variance, or `Nothing` if the sample [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
sampleStdDev_y :: Covariance -> Maybe Double
```

### populationStdDev_y

`populationStdDev_y` return the standard deviation of `y` for a population's variance, or `Nothing` if the population [count](/doc/pure-variance/0.7.0.0/Pure.Covariance/count) is 0.

```haskell
populationStdDev_y :: Covariance -> Maybe Double
```

### correlation

`correlation` is a synonym for [sampleCorrelation](/doc/pure-variance/0.7.0.0/Pure.Covariance/sampleCorrelation), which uses the usual [Bessel's correction](https://en.wikipedia.org/wiki/Bessel's_correction).

### sampleCorrelation

Apply [Pearson's Correlation Coefficient](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) to a sample's `Covariance`. Returns a value in the range `[-1,1]`, where `-1` is inverse linear correlation, `0` means uncorrelated, and `1` means positive linear correlation.

```haskell
sampleCorrelation :: Covariance -> Maybe Double
```

### populationCorrelation

Apply [Pearson's Correlation Coefficient](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient) to a population's `Covariance`. Returns a value in the range `[-1,1]`, where `-1` is inverse linear correlation, `0` means uncorrelated, and `1` means positive linear correlation.

```haskell
populationCorrelation :: Covariance -> Maybe Double
```

### data Covaried

The `Covaried` type is a newtype around a mapping from `(String,String)` to `Covariance` for storing the results of a sample analysis structures of co-varying values.

```haskell
newtype Covaried = Covaried (HashMap (String,String) Covariance)
```

See [lookupCovariance](/doc/pure-variance/0.7.0.0/Pure.Covariance/lookupCovariance) for extracting results from a `Covaried`.

### class Extract

The `Extract` class defines the method for extracting `Real` values of interest from a structure.

There are default instances for tuples, functorial wrappers, and vectors of `Real` values that are index-dependent. There are generic instances for products, sums, and records of those.

> If you know of some magic to derive the analysis of structures while maintaining type safety and performance without GHC preprocessors or higher-kinded data, I would love to see it!

The `Extract` function, `extract`, is designed/shaped to be a step in a fold for performance reasons.

```haskell
class Extract a where
  extract :: String -> a -> [(String,Double)] -> [(String,Double)]
```

### lookupCovariance

`lookupCovariance` attempts to retrieve the [Covariance](/doc/pure-variance/0.7.0.0/Pure.Covariance/data%20Covariance) at a given index pair in a [Covaried](/doc/pure-variance/0.7.0.0/Pure.Covariance/data%20Covaried) map. Order of the indices is not important.

```haskell
lookupCovariance :: String -> String -> Covaried -> Maybe Covariance
```

### covaried

`covaried` extracts the `Real` values from a structure and integrates them into an existing [Covaried](/doc/pure-variance/0.7.0.0/Pure.Covariance/data%20Covaried) analysis.

```haskell
covaried :: Extract a => a -> Covaried -> Covaried
```

### covariances

`covariances` integrates a foldable of co-varying `Real` values into a flat map of [Covariances](/doc/pure-variance/0.7.0.0/Pure.Covariance/data%20Covariance).

```haskell
covariances :: (Foldable f, Extract a) => f a -> Covaried
```

