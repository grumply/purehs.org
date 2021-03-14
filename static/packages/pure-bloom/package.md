A basic thread-safe mutable [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter) for textual values or values with a 
unique textual representation.

<pre data-try>
import Pure
import Pure.Data.Bloom

main = do
  -- construct a new filter with a 1% 
  -- false positive rate for 500 entries
  b <- new 0.01 500 

  let v = "ABCD" :: String

  add b v
  exists <- test b v

  inject body $ txt $ show exists
</pre>