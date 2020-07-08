*pure-maybe* implements two higher-order components, a producer and a consumer, that pair together to produce a rich abstraction for delay, suspense, and failure handling.

<pre data-try>
import Pure
import Pure.Maybe

main = inject body example

example = producing producer consumer
  where
    producer :: IO View
    producer = do
      delay (5 * Second) 
      pure "Resource"
    
    consumer :: Maybe View -> View
    consumer = consumingWith options id
      where
        options = 
          defaultOptions
            & suspense (500 * Millisecond) "Loading..."
            & trouble  (3 * Second) "Problems..."
</pre>

## The Producer

```haskell
producing :: IO a -> (Maybe a -> View) -> View
```

The producer takes a resource acquisition function and reifies the acquisition into a delcarative `Maybe` value that works with a supplied `Maybe resource -> View` viewer.

## The Consumer

```haskell
consumingWith :: Options -> Maybe a -> View
```

The consumer takes breakpoint options that specify what to show if the resource is slow-to-load or if the resource doesn't load. The consumer can be used as the viewer of a producer.