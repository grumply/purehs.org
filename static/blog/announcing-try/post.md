<p class="drop">
While writing tutorials, I decided it would be beneficial to demonstrate with a live editor and to make it easier for newcomers to see and experiment with working code. To that end, I'm pleased to announce the first release of [try.purehs.org](http://try.purehs.org) and automated integration within purehs.org. If you create documentation, blog posts, or tutorials, surrounding code in `<pre data-try></pre>` will turn that code into a live editor for demonstration and experimentation. 
</p>

<pre data-try>
import Pure

main = inject body "Welcome to try.purehs.org"
</pre>

All packages available in the `pure-platform` `nix-shell` are available in the live editor  with `TemplateHaskell` disabled for security reasons. Compile times are limited to 30 seconds. If you think of any security issues I've missed, please let me know. 

From a technical standpoint, the site was relatively simple to implement, but the server demands are rather steep. If you're interested in sponsoring `purehs.org`, or donating server space, please contact me <a href="mailto:sean@grump.ly">here</a>.
