## inject

Given a node, construct and inject the given [View](/packages/pure-core/latest/Pure.Data.View/data%20View) as the last child of the node.

```haskell
inject :: IsNode n => n -> View -> IO ()
```

<div class="hide">
In the browser, `inject` constructs the DOM nodes for the given [View](/packages/pure-core/latest/Pure.Data.View/data%20View), recursively, and injects the result into the given DOM node as the last child.

You will use `inject` in every Pure.hs application.

<pre data-try>
import Pure

main = inject body "Hello, World!"
</pre>

<pre data-try>
import Pure

main = do
  mb <- findByTag "body"
  for_ mb $ \b ->
    inject b "Hello, World!"
</pre>

When constructing a server or non-browser-based client, the `inject` method will still perform the same function, but will do far less work in reconciling since it doesn't need to manage an active DOM tree. See [pure-server](/packages/pure-server/latest) for examples.
</div>
