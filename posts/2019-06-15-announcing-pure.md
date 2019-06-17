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

It's not all roses, though. Pure suffers from a lack of extensive documentation,
long compile times for first-runs, and, most severely, a lack of community.

There's a lot of work awaiting. If you're interested, take a look at the 
tutorial. If you're really interested, feel free to improve the tutorial to 
benefit others.

Be excellent to each other!

[Tutorials](/tuts)
[Install](/tuts/install)
