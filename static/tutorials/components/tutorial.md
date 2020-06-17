<p class="drop">
This is the second part in the [5-Minute Series](./5-minute) of Pure.hs fundamentals. In the [first part](./basics), we learned about the `View` type and how to functionally enrich it to build a UI in a modular fashion and we built a doomsday machine.
</p>

But statics only go so far. For the rest of the journey, we'll need to know how to manage stateful views. In this tutorial, we'll cover components. Let's do it.

## The Doomsday Shroud

So you fired the missiles, eh?

The cobalt thorium G doomsday shroud, a lethal radioactive cloud, will [encircle the earth for 93 years](https://www.youtube.com/watch?v=aSlf2vB80lo?t=40). In 10 months, the surface of the earth will be as dead as the moon. I'm assuming you have a bunker? I guess we better create a countdown to let posterity know when it is safe to recolonize.

## Time

First, we need to know how to deal with time. Pure.hs has a simple [Time](/packages/pure-time/latest/Pure.Data.Time/data%20Time) type for use in non-critical situations. It doesn't make an effort to deal with leap years, days, or seconds. But, in most situations, it is convenient and sufficient.[^1] 

Let's figure out when the doomsday shroud will have dissipated.

<pre data-try>
import Pure

main = do
  now <- time
  inject body $ fromTxt $
    "The planet will be habitable on "
      <> toPrettyDate (now + Years 93 0)
</pre>

[Years](/packages/pure-time/latest/Pure.Data.Time/pattern%20Years) lets us construct a `Time` value in years. There are similar patterns for [Months](/packages/pure-time/latest/Pure.Data.Time/pattern%20Months), [Weeks](/packages/pure-time/latest/Pure.Data.Time/pattern%20Weeks), [Days](/packages/pure-time/latest/Pure.Data.Time/pattern%20Weeks), etc.... Note that `Years` and `Months` are approximations.

We can also inspect `Time` values with the same patterns, and we can chain them together since the second component of the pattern is a remainder.

<pre data-try>
import Pure

main = do
  Years y (Months m (Days d _)) <- time
  inject body $ txt $ 
    show y ++ " years " ++
    show m ++ " months " ++ 
    show d ++ " days since the beginning of the unix epoch" 
</pre>

But this approach is limited. The page is still static, and we want a countdown. A few generations from now when social order has crumbled and propagation of generational knowledge has faltered, people might not know how to refresh a web page. Let's create a dynamic live countdown that always shows the current time until the doomsday shroud has dissipated. Let's hope people can still read.

## Components

The Pure.hs core [View](/packages/pure-core/latest/Pure.Data.View/data%20View) type lets us construct a stateful view called a component. We'll need to use a componnent to dynamically update our countdown.

To pair with the component view, Pure.hs implements a method for dynamically managing these nested contexts. In the browser, this means updating the page when the component's `View` changes. This is often called reconciliation - synchronizing the browser's live representation of the `View` with that of the more recent representation that the component maintains. The `inject` method that we've been using will make sure that any components are fully managed and reconcile themselves with the page as they update.

<pre data-try>
import Control.Concurrent
import Pure

main = inject body (countdown ())

data Countdown = Countdown
  { now  :: !Time
  , safe :: !Time
  }

countdown :: () -> View
countdown = Component $ \self -> 
  let
    clock = do
      delay Day
      t <- time
      modify self $ \_ cd -> cd { now = t }
      clock

  in
    def
      { construct = do
        forkIO clock
        now <- time
        let safe = now + 93 * Year
        pure Countdown {..}

      , render = \() Countdown {..} ->
        let Years y (Months m (Days d _)) = safe - now
        in txt $ show y ++ " years " ++
                 show m ++ " months " ++
                 show d ++ " days until the doomsday shroud has dissipated."
      }
</pre>

This may look a little daunting, but once you understand the structure, it's relatively simple. Let's walk through it. 

### The State

```haskell
data Countdown = Countdown
  { now  :: !Time
  , safe :: !Time
  }
```

We've defined a core state type that holds two time values: 

* `now` tracks the most recent tick of a clock
* `safe` tracks the time at which we expect the shroud to have dissipated 

We'll update the `now` value near-continuously, but the `safe` value will be created, but never modified.

### The Component

```haskell
countdown :: () -> View
countdown = Component $ \self ->
```

Everything within the configuration of the component has access to the component via `self`, allowing for updates and inspection. When an update is performed with `modify self`, the modification is sent to a queue to be performed batched with any other updates that happen to occur around the same time.

### The Clock

```haskell
clock = do
  delay Day
  t <- time
  modify self $ \_ cd -> cd { now = t }
  clock
```

Our clock simply waits for a day and then updates the `now` field of our state and then restarts.

### The Construction

```haskell
    def
      { construct = ...
      , render = ...
      }
```

We construct a component with `def`, which initializes the component with reasonable field defaults[^2]. We use the `construct` field to initialize the component's state, and the render field defines the on-page `View` of the component.

> Note that the `render`ed view can be any valid `View` including other components or even `Null`, the default.

### The Initialization

```haskell
construct = do
  forkIO clock
  now <- time
  let safe = now + 93 * Year
  pure Countdown {..}
```

On startup, our component will start the clock in a new thread, and initialize the countdown state.

### The View

```haskell
render = \_ Countdown {..} ->
  let Years y (Months m (Days d _)) = safe - now
  in txt $ show y ++ " years " ++
           show m ++ " months " ++
           show d ++ " days until the doomsday shroud has dissipated."
```

The `render` function projects the current `Countdown` state into a textual `View`.

### Separation of Concerns

Managing two `Time` values isn't especially difficult, but if all of our state was managed by one component, things could quickly get out of hand. Let's nip this in the bud.

<pre data-try>
import Control.Concurrent
import Pure

main = inject body (clock ())

clock :: () -> View
clock = Component $ \self ->
  let
    clock = do
      delay Day
      t <- time
      modify self $ \_ -> const t
      clock
  in
    def 
      { construct = forkIO clock *> time
      , render = \_ now -> countdown now
      }

countdown :: Time -> View
countdown = Component $ \self -> 
  def
    { construct = fmap ((93 * Year) +) time
    , render = \now safe ->
      let Years y (Months m (Days d _)) = safe - now
      in txt $ show y ++ " years " ++
               show m ++ " months " ++
               show d ++ " days until the doomsday shroud has dissipated."
    }
</pre>

Components take properties *from above*, allowing for information passing when nesting. This communication channel is what gives Pure.hs much of its power: linking together dynamic components with an implicit flow of information in a declarative fashion. Here, the `clock` will tick and implicitly notify the `countdown` that the time has changed when it is re-rendered. 

In addition to this added compositionality, `clock` is now reusable and can easily be made configurable. Consider this simple extension that allows for choosing an update interval.

```haskell
clock :: (Time,Time -> View) -> View
clock = Component $ \self ->
  let
    clock d = do
      delay d
      t <- time
      modify self $ \_ -> const t
      clock
  in
    def 
      { construct = do
        (d,_) <- ask self 
        forkIO (clock d) 
        time
      , render = \(_,f) now -> f now
      }
```

## The Mistake

I'm afraid that since you've already made the mistake of enshrouding the earth in a sphere of deadly radiation, you'll make the mistake of restarting the countdown. Let's hardcode the end date and calculate relative to that. That way, refreshing the page won't reset the countdown. 

<pre data-try>
import Control.Concurrent
import Pure

main = do
  let Just safe = fromPrettyDate @Txt "May 29, 2113"
  inject body (countdown safe)

clock :: Time -> View
clock = Component $ \self ->
  let
    clock = do
      delay Day
      t <- time
      modify self $ \_ -> const t
      clock
  in
    def 
      { construct = forkIO clock *> time
      , render = \safe now -> countdown safe now
      }

countdown :: Time -> Time -> View
countdown now safe =
  let Years y (Months m (Days d _)) = safe - now
  in txt $ show y ++ " years " ++
           show m ++ " months " ++
           show d ++ " days until the doomsday shroud has dissipated."
</pre>

Much better. Now you needn't worry about restarting the countdown - it'll maintain the correct time across restarts.

## What have we learned?

Time is hard. Using a relatively simple approach to `Time`, we can get something that works reasonably well in many cases. Using components, we can construct views that manage state and re-render when that state updates. 

We learned that we can separate components into hierarchies with information flowing downwards. But, not all information flow is this simple and not all components are willing to pass our informationg through for us, so we'll see how to work with non-linear information flow in later tutorials.

Now that we've seen how state is managed, let's learn how to put dynamic views together to create more complex applications. Up next, we'll learn how to use *The Elm Architecture* to fully integrate an application in the [Applications Tutorial](./applications). After that, we'll apply some of the same abstractions to server-side development in the [Servers Tutorial](./servers).

[^1]: [time](http://hackage.haskell.org/package/time) and its `Data.Time.UTCTime` are available, but are less pedagogically convenient.
[^2]: Components have other [fields](/packages/pure-core/latest/Pure.Data.View/data%20Comp), as well.

<div class="prev">
[< Prev](./basics)
</div>

<div class="next">
[Next >](./applications)
</div>