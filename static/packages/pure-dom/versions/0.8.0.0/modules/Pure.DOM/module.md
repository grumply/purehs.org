## inject

Given a node, construct and inject the given [View](/packages/pure-core/latest/Pure.Data.View/data%20View) as the last child of the node. 

```haskell
inject :: IsNode n => n -> View -> IO ()
```

<div class="hide">
In the browser, *inject* constructs the DOM nodes for the given *View*, recursively, and injects the result into the given DOM node as the last child.

You will use *inject* in every Pure.hs application.

<pre data-try>
import Pure

main = inject body "Hello, World!"
</pre>

<p></p>

<pre data-try>
import Pure

main = findByTag "body" >>= maybe nothing just
  where
    nothing = print "&lt;body> not found"
    just b  = inject b "Hello, World!"
</pre>

When constructing a server or non-browser-based client, the *inject* method will still perform the same function, but will do far less work in reconciling since it doesn't need to manage an active DOM tree. See [pure-server](/packages/pure-server/latest) for examples.

### How It Works

Component views are constructed and forked to self-manage with a reconciliation algorithm. The reconciliation algorithm manages three [View](/packages/pure-core/latest/Pure.Data.View/data%20View) trees: the previous representation, a modified live representation, and a newly-produced representation. When a view changes, the new view is compared with the previous view, recurisvely, looking for changes. The reconciler relies on referrential equality of the produced views, previous and new, in many cases reducing the cost of reconciliation to a single pointer equality check.

When checking for feature changes, including attribute, property, class, style, and event listeners, the same approach, of referrential equality checking, is used to check for changes to subsets of the features, including attributes, properties, classes, and styles, before doing the same with individual attributes, properties, classes, and styles. Eventually, with attributes, properties, classes, and styles, textual equality checks must be performed when changes are found with the referrential equality checking approach. This reconciler attempts to perform as little work as possible at every step to avoid unnecessary expensive equality checks.

When changes are found, the work performed to reconcile the views is first planned with the live representation and then queued to commit the render-associated changes together with any other commits that that component or other components have queued. During the planning stage, DOM nodes may be generated and have features attached to them, pre-commit, to improve render and paint times in the browser. The work performed during commit is generally DOM node replacement, and direct feature application - quick changes, with all of the diffing performed pre-commit.

In certain cases, especially when a DOM element's tag changes or the *View* constructor changes, signifying a major change to the application, the strategy changes from reconciliation to remove-and-replace. The replace performs all the work of constructing the new nodes in the planning phase, and the commit phase for that change becomes a simple node swap. When a replace is performed, some of the work of cleanup is deferred to an idle callback to reduce some immediate costs of large application changes.

Similar to many virtual DOM reconcilers, when reconciling keyed children, only simple single-node inserts, single-node removals, and swapping are performed especially efficiently. When multiple nodes are inserted sequentially, tails are discarded and re-built. An approach to collection and re-use was attempted, but slowed the algorithm too much in common cases; this may be an avenue to re-visit at some point in the future, as the benefits are rather nice, including in some cases of complex animations that require node re-use. As it stands, and as with most virtual DOM reconcilers, to maintain performance of diffing keyed children, be sure that nodes are added or removed one-at-a-time, rather than in batches.

Forked threads manage the commit queue and the idle work queue, performing actions in upcoming animation frames to reduce staggering of updates, and upcoming idle periods to reduce load during updates, respectively.

Components add hooks to guarantee actions can be performed before and after reconciliation, for managing complex views. See [Comp](/packages/pure-core/latest/Pure.Data.View/data%20Comp) for more.

When *inject* is used server-side, much of the work of reconciling is reduced to memory assignment, since there is no DOM to be managed and diffing is unnecessary. However, the hooks supplied by components are run in the same order as in the browser; it is expected that running a client application server-side produces and runs actions in the same order, up to thread synchronization. In the general case, the reconciliation algorithm is much more efficient server-side.
</div>