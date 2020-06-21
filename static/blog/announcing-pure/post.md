<p class="drop">
I am pleased to announce the first public release of Pure.hs, a library ecosystem for web applications, servers, and services. In the process of developing Pure.hs, I implemented dozens of libraries - some useful, some not - to poke and prod at the boundaries and capabilities implied by various core abstractions. Over time, to improve the expressiveness, performance, and concision, I rewrote and refined Pure.hs many times and incorporated ideas from many web frameworks, including ReactJS, Elm, and Servant. Starting, originally, from imperative origins, Pure.hs evolved into a reactive framework, before finally finding its footing in a reactive/declarative hybrid that focuses on hierachies of dependent contexts. The version seen today is vastly different from the original, and hopefully something you'll enjoy using.
</p>

Pure.hs is still an active testbed for Haskell web framework ideas, but has mostly settled in the last year. Pure.hs uses a well-optimized rendering and reconciliation algorithm and can match or exceed the performance of top-tier web frameworks like React, Vue, and Angular.

![Some slightly outdated benchmarks, but still representative.](/static/results-small.png)

Check out the core features that [Pure.hs offers](/about).

This website is written entirely in Pure.hs. Check out the source [here](https://github.com/grumply/purehs.org).

If you're interested, take a look at the [tutorials](/tut). If you're really interested, feel free to [improve](https://github.com/grumply/purehs.org/pulls) the tutorials to benefit others.

If you've got questions, check out the [discourse](http://discourse.purehs.org).