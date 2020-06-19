`pure` is a frontend-focused development meta-library that re-exports common functionality from:

* [pure-core](/packges/pure-core/latest)
* [pure-default](/packges/pure-default/latest)
* [pure-dom](/packges/pure-dom/latest)
* [pure-events](/packges/pure-events/latest)
* [pure-html](/packges/pure-html/latest)
* [pure-lifted](/packges/pure-lifted/latest)
* [pure-state](/packges/pure-state/latest)
* [pure-styles](/packges/pure-styles/latest)
* [pure-theme](/packages/pure-theme/latest)
* [pure-time](/packges/pure-time/latest)
* [pure-txt](/packges/pure-txt/latest)

When developing a Pure application, most modules that use Pure functionality will import the `Pure` module. An alternative that re-exports much of this meta-library is [pure-elm](/packges/pure-elm/latest).

<pre data-try>
import Pure

main = inject body "Hello, World!"
</pre>