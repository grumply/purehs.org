
## normal

Generate a normally distributed random variate with the given mean and standard deviation.

```haskell
normal :: Double -> Double -> Generator Double
normal mean stdDev = (\x -> mean + stdDev * x) <$> standard
```

<div class="hide">
<pre data-try>
import Pure hiding (list,normal,head)
import Pure.Random (Seed,newSeed,list)
import Pure.Random.Distributions (normal)

import Data.Map as Map (Map,fromList,lookup)
import Data.List (groupBy,sort,take,head,length)

main = do
  rs <- Main.randoms 50 12 <$> newSeed
  inject body $
    Div <| Themed @Bars |>
      [ Div <| Height (fromIntegral r px)
      | n <- [0..99]
      , let r = maybe 0 id (Map.lookup n rs)
      ]

randoms :: Double -> Double -> Seed -> Map Int Int      
randoms mean sd = fromList . count . group . trim . create
  where
    count  = fmap (\xs -> (head xs,length xs))
    group  = groupBy (==) . fmap round . sort
    trim   = take 10000
    create = list (normal mean sd)
    
data Bars
instance Theme Bars where
  theme c =
    is c do
      display     =: flex
      align-items =: flex-end

      has (tag Div) do
        width            =: 3px
        padding          =: 1px
        background-color =: blue
</pre>
</div>

## standard

Generate a normally distributed random variate with zero mean and unit variance.

```haskell
standard :: Generator Double
standard = normal 0 1
```

<div class="hide">
<pre data-try>
import Pure hiding (list,head)
import Pure.Random (Seed,newSeed,list)
import Pure.Random.Distributions (standard)

import Data.Map as Map (Map,fromList,lookup)
import Data.List (groupBy,sort,take,head,length)

main = do
  rs <- Main.randoms <$> newSeed
  inject body $
    Div <| Themed @Bars |>
      [ Div <| Height (fromIntegral r px)
      | n <- [-30,-29..30]
      , let r = maybe 0 id (Map.lookup n rs)
      ]

randoms :: Seed -> Map Int Int      
randoms = fromList . count . group . trim . create
  where
    count  = fmap (\xs -> (head xs,length xs))
    group  = groupBy (==) . fmap (round . (* 10)) . sort
    trim   = take 5000
    create = list standard
    
data Bars
instance Theme Bars where
  theme c =
    is c do
      display     =: flex
      align-items =: flex-end

      has (tag Div) do
        width            =: 3px
        padding          =: 1px
        background-color =: blue
</pre>
</div>

## exponential

`exponential` is the continuous process counterpart to the discrete process represented by [geometric](Pure.Random.Distributions/geometric) random generation. Similar to the way in which a [geometric](Pure.Random.Distributions/geometric) random variate corresponds to a random sampling of a probability space representing the number of failures before a success, `exponential` corresponds to a random sampling of a probability space representing the time before a state transition within a continuous process.

Effectively, given a rate of occurence, generate a random variate representing a time between events for a process whose events are continuous and fully independent. 

<div class="warn">
Be sure that your events correspond to a process in which events are truly continuous and independent. See [Poisson point process](https://en.wikipedia.org/wiki/Poisson_point_process) for more information.
</div>

<div class="note">
Returns a value in the range [0..] representing the time before an event.

This can be useful for generating and scheduling events at predictable, but random, intervals, or scheduling the lifetime of an event.

See [truncatedExp](Pure.Random.Distributions/truncatedExp) for a method of limiting the range of produced variates.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (events,table)
import Pure.Data.JSON (pretty)
import Pure.Random (Seed,newSeed,generate)
import Pure.Random.Distributions (exponential)
import Data.List (take,sum,unfoldr,zipWith)

main = do
  now  <- localizeTime =<< time
  seed <- newSeed
  let 
    
    es = take 50 (events rate now seed)
      where
        rate = 1 / (60 * 20) 

    avg = sum deltas / 49
      where
        deltas = zipWith (flip (-)) es (tail es)

  inject body (table es avg)

-- Generate a series of independent events at a 
-- given average rate offset by a starting time. 
events :: Double -> Time -> Seed -> [Time]
events rate t0 s0 = unfoldr go (s0,t0)
  where
    go (s,i) =
      let 
        (s',e) = generate (exponential rate) s
        millis = round (e * 1000)
        i' = i + Milliseconds millis 0
      in
        Just (i',(s',i'))

table :: [Time] -> Time -> View
table es (Minutes m _) =
  Div <||>
    [ Div <||>
      [ Div <||> [ fromTxt (formatTime "%r" e) ]
      | e <- es
      ] 
    , Div <||>
      [ txt ("Average: " <> toTxt m <> " minuntes")
      ]
    ]
</pre>
</div>


## truncatedExp

`truncatedExp` is the continuous process counterpart to the discrete process represented by [geometric](Pure.Random.Distributions/geometric) random generation. Similar to the way in which a [geometric](Pure.Random.Distributions/geometric) random variate corresponds to a random sampling of a probability space representing the number of failures before a success, `truncatedExp` corresponds to a random sampling of a probability space representing the time before a state transition within continuous process, but bounded by a range.

Effectively, given a rate of occurence and a range of allowed results, generate a random variate representing a time between events for a process whose events are continuous and fully independent that falls within the specified range. 

<div class="warn">
Be sure that your events correspond to a process in which events are truly continuous and independent. See [Poisson point process](https://en.wikipedia.org/wiki/Poisson_point_process) for more information.

Be careful with the range as it can invalidate important process assumptions! Importantly, the average will shift based on the supplied range.
</div>

<div class="note">
Returns a value in the specified range representing the time between two events.

This can be useful for generating and scheduling events at predictable, but random, intervals.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (events)
import Pure.Random (Seed,newSeed,generate)
import Pure.Random.Distributions (truncatedExp)
import Data.List (take,sum,unfoldr,zipWith)

main = do
  now  <- localizeTime =<< time
  seed <- newSeed
  let 
    
    es = take 50000 (events now seed)

    Minutes a _ = sum deltas / 49999
      where
        deltas = zipWith (flip (-)) es (tail es)
    
  -- We have generated enough events that we
  -- can observe the shift in the average above our 
  -- specified 20 minute rate due to the minimum
  -- bound placed on our random variates.
  inject body $
    txt ("Average: " <> toTxt a <> " minutes")

-- Generate a series of independent events with
-- an average interval of 1200 seconds and a
-- minimum interval of 500 seconds.
events :: Time -> Seed -> [Time]
events t0 s0 = unfoldr go (s0,t0)
  where
    go (s,i) =
      let inf = 1/0
          (s',e) = generate (truncatedExp (1 / 1200) (500,inf)) s
          i' = i + Seconds (round e) 0
      in Just (i',(s',i'))
</pre>
</div>

## geometric0

Given a probability of success in a binomial trial (like a coin flip), generate a random variate corresponding to a random sampling of a probability space representing the number of failures before a success for such a weighted event. 

Effectively, generate a random variate representing a result of a series of binomial trials with a known event probability without running the trials.

```haskell
geometric0 :: Double -> Generator Int
```

<div class="note">
Returns a value in the range [0..] representing the number of failures before the first success.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (list)
import Pure.Random (newSeed,list)
import Pure.Random.Distributions (geometric0)

main = do
  rs <- list (geometric0 0.25) <$> newSeed
  let ts = take 100000 rs
      x  = fromIntegral (sum ts) / 100000
  inject body (txt @Double x)
</pre>
</div>

## geometric1

Given a probability of success in a binomial trial (like a coin flip), generate a random variate corresponding to a random sampling of a probability space representing the number of trials up to and including a success for such a weighted event. 

Effectively, generate a random variate representing a result of a series of binomial trials with a known event probability without running the trials.

```haskell
geometric1 :: Double -> Generator Int
geometric1 = fmap succ . geometric0
```

<div class="note">
Returns a value in the range [1..] representing the number of flips up to and including the first success.
</div>

<div class="hide">
<pre data-try>
import Pure hiding (list)
import Pure.Random (newSeed,list)
import Pure.Random.Distributions (geometric1)

main = do
  rs <- list (geometric1 0.33) <$> newSeed
  let ts = take 100000 rs
      x  = fromIntegral (sum ts) / 100000
  inject body (txt @Double x)
</pre>
</div>

## gamma

Generate a random variate from a [gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution), given a [shape parameter](https://en.wikipedia.org/wiki/Shape_parameter) ***k*** and a [scale parameter](https://en.wikipedia.org/wiki/Scale_parameter) ***θ***.


## chiSquare

Generate a random variate from a [chi-square distribution](https://en.wikipedia.org/wiki/Chi-square_distribution) with ***k*** [degrees of freedom](https://en.wikipedia.org/wiki/Degrees_of_freedom_(statistics)).

## beta

Generate a random variate from a [beta distribution](https://en.wikipedia.org/wiki/Beta_distribution) with two [shape parameters](https://en.wikipedia.org/wiki/Shape_parameter) ***α*** and ***β***.

## dirchlet

A generalization of [beta](Pure.Random.Distributions/beta) to an arbitrary number of variables given in a traversable container.

## bernoulli

Generate a random `Bool` by running a [Bernoulli trial](https://en.wikipedia.org/wiki/Bernoulli_trial) with a given success rate - a weighted coin flip.

## categorical

A generalization of [bernoulli](Pure.Random.Distributions/bernoulli) to an arbitrary set of choices, given as a set of weights in a vector. The result of a categorical trial is an index into the weight vector.

## logCategorical

A generalization of [bernoulli](Pure.Random.Distributions/bernoulli) to an arbitrary set of choices, given as a set of weights in log domain in a vector. The result of a log categorical trial is an index into the weight vector.
