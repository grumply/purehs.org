I'm please to announce [pure-variance](/packages/pure-variance/latest), a library with an easy-to-use implementation of variance and covariance algorithms.

For variance, *pure-variance* implements [Welford's online variance algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm) with the *Semigroup* and *Monoid* instances for [Variance](pure-variance/0.8.0.0/Pure.Variance/data%20Variance) implementing the [parallel variant](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm). 

For covariance, *pure-variance* implements the [similar approach](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online) with the *Semigroup* and *Monoid* instances for [Covariance](pure-variance/0.8.0.0/Pure.Covariance/data%20Covariance) implementing the [parallel variant](https://dl.acm.org/citation.cfm?doid=3221269.3223036).

## Functional Interface

Both [Pure.Variance](/packages/pure-variance/0.8.0.0/Pure.Variance) and [Pure.Covariance](/packages/pure-variance/0.8.0.0/Pure.Covariance) implement similar interfaces. There is a functional interface that uses accessor functions to pull values from a larger structure for analysis.

<pre data-try>
import Pure hiding (x,y)
import Pure.Variance
import Pure.Covariance

data Point = Point { x :: Double, y :: Double }

points = take 10 [ Point x (x^2) | x <- [1..] ]

main = inject body $ txt $ show
  ( variance (varies x points)
  , stdDev (varies x points)
  , covariance (covaries x y points)
  , correlation (covaries x y points)
  )
</pre>

Since [varies](/packages/pure-variance/0.8.0.0/Pure.Variance/varies) and [covaries](/packages/pure-variance/0.8.0.0/Pure.Covariance/covaries) implement on-line algorithms, the [Variance](/packages/pure-variance/0.8.0.0/Pure.Variance/data%20Variance) and [Covariance](/packages/pure-variance/0.8.0.0/Pure.Covariance/data%20Covariance) structures can be kept alive to be amended and queried for richer low-overhead algorithm implementations in situations where continual full-pass analyses are prohibitively expensive. 

## Generic Interface

There is also a generic interface that automatically analyzes a full structure of arbitrary fields, but is stringly-typed. This will fail in the presence of duplicate names, but is nice for a quick-and-dirty analysis. Once intuition is gained about the structure and the connections in your data, it's best to switch to the functional approach, as above.

Analyses are derivable for custom structures via generics. [Vary](/packages/pure-variance/0.8.0.0/Pure.Variance/class%20Vary) for variance, and [Covary](/packages/pure-variance/0.8.0.0/Pure.Covariance/class%20Covary) for covariance.

<pre data-try>
{-# language DeriveAnyClass #-}
import Pure hiding (x,y)
import Pure.Variance
import Pure.Covariance
import GHC.Generics

data Point = Point { x :: Double, y :: Double }
  deriving (Generic,Vary,Covary)

points = take 10 [ Point x (x^2) | x <- [1..] ]

main = inject body $ txt $ show
  ( fmap variance (lookupVariance "x" (variances points))
  , fmap variance (lookupVariance "y" (variances points))
  , fmap covariance (lookupCovariance "x" "y" (covariances points))
  )
</pre>