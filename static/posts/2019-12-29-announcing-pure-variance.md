# Announcing pure-variance

[pure-variance](/doc/pure-variance/0.7.0.0) implements both online and parallel-online algorithms of variance and covariance with generic machinery for deriving the analysis of arbitrary type topologies.

```haskell
> import Pure.Variance
> import Pure.Covariance

> points = take 5 [ (x,x^2) | x <- [1..] ]
points :: [(Double,Double)]

> varx = varies fst points
varx :: Variance  

> cov = covaries fst snd points
cov :: Covariance

> variance varx
Just 2.5

> stdDev varx
Just 9.669539802906858

> covariance cov
Just 15.0

> correlation cov
Just 0.9811049102515929
```

Since [varies](/doc/pure-variance/0.7.0.0/Pure.Variance/varies) and [covaries](/doc/pure-variance/0.7.0.0/Pure.Covariance/covaries) implement on-line algorithms, the [Variance](/doc/pure-variance/0.7.0.0/Pure.Variance/data%20Variance) and [Covariance](/doc/pure-variance/0.7.0.0/Pure.Covariance/data%20Covariance) structures can be kept alive to be amended and queried for richer low-overhead algorithm implementations in situations where continual full-pass analyses are prohibitively expensive.

The generic approach is similar, but lacks the type-safety of the function approach, above. The generic analyses map all values into a flattened structure. This will fail in the presence of duplicate names, but is nice for a quick-and-dirty analysis. Once intuition is gained about the structure and the connections in your data, it's best to switch to the functional approach, as above.

```haskell
> points = take 5 [ (x,x^2,x^3) | x <- [1..] ]
points :: [(Double,Double,Double)]

> var = variances points
var :: Variances  

> cov = covariances points
cov :: Covariances

-- lookup the variance for the first element of the tirple
> lookupVariance "1" var >>= variance
Just 1.5811388300841898

> lookupVariance "2" var >>= stdDev
Just 9.669539802906858

-- lookup the covariance of the first and second elements of the triple
> lookupCovariance "1" "2" cov >>= covariance
Just 15.0

> lookupCovariance "1" "3" cov >>= correlation
Just 0.9431175138077005
```

Analyses are derivable for custom structures via generics. [Vary](/doc/pure-variance/0.7.0.0/Pure.Variance/class%20Vary) for variance, and [Covary](/doc/pure-variance/0.7.0.0/Pure.Covariance/class%20Covary) for covariance.

```haskell
> data Point = Point { x :: Double, y :: Double } deriving (Generic,Vary,Covary)

> points = take 5 [ Point x (x ^ 2) | x <- [1..] ]
points :: [Point]

> var = variances points
var :: Variances  

> cov = covariances points
cov :: Covariances

> lookupVariance "x" var >>= variance
Just 2.5

> lookupCovariance "x" "y" cov >>= correlation
Just 0.9811049102515929
```


