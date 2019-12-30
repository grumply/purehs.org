# Announcing pure-variance

[pure-variance](/doc/pure-variance/0.7.0.0) implements both online and parallel-online algorithms of variance and covariance with generic machinery for deriving the analysis of arbitrary type topologies.

```haskell
𝝀 import Pure.Variance
𝝀 import Pure.Covariance

𝝀 points = [ (1,2), (2,3), (3,4), (4,5) ]

𝝀 varies fst points
Variance {vCount = 4.0, vMean = 2.5, vMean2 = 5.0, vMinimum = 1.0, vMaximum = 4.0}

𝝀 varies snd points
Variance {vCount = 4.0, vMean = 3.5, vMean2 = 5.0, vMinimum = 2.0, vMaximum = 5.0}

𝝀 covaries fst snd points
Covariance {cCount = 4.0, cMeanx = 2.5, cMeany = 3.5, cMeanx2 = 5.0, cMeany2 = 5.0, cC = 5.0}

𝝀 populationCorrelation (covaries fst snd points)
Just 0.9999999999999998
```

Since [varies](/doc/pure-variance/0.7.0.0/Pure.Variance/varies) and [covaries](/doc/pure-variance/0.7.0.0/Pure.Covariance/covaries) implement on-line algorithms, the [Variance](/doc/pure-variance/0.7.0.0/Pure.Variance/data%20Variance) and [Covariance](/doc/pure-variance/0.7.0.0/Pure.Covariance/data%20Covariance) structures can be kept alive to be amended and queried for richer low-overhead algorithm implementations in situations where continual full-pass analyses are prohibitively expensive.

The generic approach is similar, but lacks the type-safety of the function approach, above. The generic analyses map all values into a flattened structure. This will fail in the presence of duplicate names, but is nice for a quick-and-dirty analysis. Once intuition is gained about the structure and the connections in your data, it's best to switch to the functional approach, as above.

```haskell
𝝀 points = [ (1,2,3), (2,3,4), (3,4,5), (4,5,6), (5,6,7) ]

𝝀 varieds points
  "1" =>
    Variance {vCount = 5.0, vMean = 3.0, vMean2 = 10.0, vMinimum = 1.0, vMaximum = 5.0}
  "2" =>
    Variance {vCount = 5.0, vMean = 4.0, vMean2 = 10.0, vMinimum = 2.0, vMaximum = 6.0}
  "3" =>
    Variance {vCount = 5.0, vMean = 5.0, vMean2 = 10.0, vMinimum = 3.0, vMaximum = 7.0}

𝝀 covarieds points
  ("2","1") =>
    ,Covariance {cCount = 5.0, cMeanx = 4.0, cMeany = 3.0, cMeanx2 = 10.0, cMeany2 = 10.0, cC = 10.0}
  ("3","1") =>
    ,Covariance {cCount = 5.0, cMeanx = 5.0, cMeany = 3.0, cMeanx2 = 10.0, cMeany2 = 10.0, cC = 10.0}
  ("3","2") =>
    Covariance {cCount = 5.0, cMeanx = 5.0, cMeany = 4.0, cMeanx2 = 10.0, cMeany2 = 10.0, cC = 10.0}
```

Analyses are derivable for custom structures via generics. [Vary](/doc/pure-variance/0.7.0.0/Pure.Variance/class%20Vary) for variance, and [Extract](/doc/pure-variance/0.7.0.0/Pure.Covariance/class%20Extract) for covariance.

```haskell
𝝀 data Point = Point { x :: Double, y :: Double } deriving (Generic,Vary,Extract)

𝝀 points = [ Point 1 2, Point 2 3, Point 3 4 ]

𝝀 varieds points
  "x" =>
    Variance {vCount = 3.0, vMean = 2.0, vMean2 = 2.0, vMinimum = 1.0, vMaximum = 3.0}
  "y" =>
    Variance {vCount = 3.0, vMean = 3.0, vMean2 = 2.0, vMinimum = 2.0, vMaximum = 4.0}

𝝀 covarieds points
  ("y","x") =>
    Covariance {cCount = 3.0, cMeanx = 3.0, cMeany = 2.0, cMeanx2 = 2.0, cMeany2 = 2.0, cC = 2.0}
```


