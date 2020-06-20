This package implements incremental and incremental parallel algorithms for [variance](https://en.wikipedia.org/wiki/Variance) and [covariance](https://en.wikipedia.org/wiki/Covariance) as well as generic machinery for deriving analyses for arbitrary *Generic* structures containing varying and covarying values.

<div class="info">
*pure-variance* uses [Welford's online variance algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm) with the *Semigroup* and *Monoid* instances for [Variance](pure-variance/latest/Pure.Variance/data%20Variance) implementing the [parallel variant](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm). The [similar approach for covariance](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online) is used with the *Semigroup* and *Monoid* instances for [Covariance](pure-variance/latest/Pure.Covariance/data%20Covariance) implementing the recent numerically stable [Schubert and Gertz parallel variant](https://dl.acm.org/citation.cfm?doid=3221269.3223036).
</div>

*pure-variance* has two approaches to population analysis: one may manually, and type-safely, analyze extraction functions or generically, and *stringly*-typed, analyze arbitrary *Generic* types containing varying or co-varying values.

