## consuming 

Consume a maybe value with a simple view function. No frills.

```haskell
consuming :: (a -> View) -> Maybe a -> View
consuming = maybe Null
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Maybe

main = inject body test

test = producing (pure "Result") (consuming id)
</pre>
</div>

## consumingWith

Consume a maybe value with [Options](Pure.Maybe.Consumer/data%20Options). 


```haskell
consumingWith :: Options -> (a -> View) -> Maybe a -> View
```

<div class="hide">
<pre data-try>
import Pure
import Pure.Maybe

main = inject body test

test = producing slowProducer (consumingWith options id)
  where
    slowProducer = do
      delay (5 * Second)
      pure "Result"

    options = defaultOptions
            & suspense (500 * Millisecond) "Loading..."
            & trouble  (3 * Second) "Problems..."
</pre>
</div>

## data Options 

Options describing how the [consumingWith](Pure.Maybe.Consumer/consumingWith) component should handle slow-to-load resources.

```haskell
data Options = Options {}
```

<div class="hide">
Use the configuration functions to specify options.

  * [defaultOptions](Pure.Maybe.Consumer/defaultOptions)
  * [delaying](Pure.Maybe.Consumer/delaying)
  * [suspense](Pure.Maybe.Consumer/suspense)
  * [trouble](Pure.Maybe.Consumer/trouble)
</div>

## defaultOptions

Default [Options](Pure.Maybe.Consumer/data%20Options)

```haskell
defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing
```

<div class="hide">
Use the configuration functions to specify options.

  * [delaying](Pure.Maybe.Consumer/delaying)
  * [suspense](Pure.Maybe.Consumer/suspense)
  * [trouble](Pure.Maybe.Consumer/trouble)
</div>

## delaying

Specify a [Time](/packages/pure-time/latest/Pure.Data.Time/data%20Time) to delay showing a result even if a resource has arrived. This options is useful for avoiding flashes of content or the guarantee that a suspense view is shown. This is a hostile approach, and should be used very carefully.

```haskell
delaying :: Time -> View -> Options -> Options
```
    
## suspense

Specify a suspense [View](/packages/pure-core/latest/Pure.Data.View/data%20View) to display after given [Time](/packages/pure-time/latest/Pure.Data.Time/data%20Time). Often a loading spinner, or similar.

```haskell
suspense :: Time -> View -> Options -> Options
```

## trouble

Specify a trouble [View](/packages/pure-core/latest/Pure.Data.View/data%20View) to display after a given [Time](/packages/pure-time/latest/Pure.Data.Time/data%20Time). Often a message saying there is trouble acquiring a resource. If the resource loads after the trouble view is rendered, the resource is still rendered; this is not for use as a failure mode, but not a trouble.

```haskell
trouble :: Time -> View -> Options -> Options
```