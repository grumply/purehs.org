# Announcing Pure

After 2 years of experimentation, I'm pleased to announce the first public
release of Pure, a library ecosystem for web applications, web servers, and web
services.

In the process of developing Pure, I implemented a number of libraries to poke
and prod at the boundaries implied by the abstractions. Over time, to improve 
the expressiveness and concision, I rewrote and refined the abstractions and 
incorporated ideas from React and Elm. A half-dozen nearly independent 
implementations were tried before settling on this version.

Pure uses a best-in-class rendering and reconciliation algorithm that avoids 
performing work within animation frames, allowing for efficiently multitasked 
execution and reconciliation with a short synchronizing commit phase. Having 
been been tuned for performance, Pure can match the performance of first tier 
web frameworks like React, Vue, and Angular.

If you're interested, take a look at the tutorials. If you're really interested,
feel free to improve the tutorials to benefit others.

[Tutorials](/tuts)
[Install](/tuts/install)

If you've got questions or want to help answer questions, check out the [discourse](http://discourse.purehs.org).

Be excellent to each other!
