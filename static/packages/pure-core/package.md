> #### Warning 
>
> This is an internal package. This is not the best place to learn how to develop with Pure. Consider starting with the [5-Minute Series](/tutorials/5-minute).
> 
> Importantly, [Pure.Data.View](latest/Pure.Data.View) contains methods and types that are internal to Pure, but [Pure.Data.View.Patterns](latest/Pure.Data.View.Patterns) contains patterns and classes that are commonly used to construct [Views](latest/Pure.Data.View/data%20View).

This package implements low-level types and methods for constructing and manipulating dependent and hierarchical computational contexts. When implementing user interfaces, the dependent evaluation is of nested and hierarchical views with shared state and environment. When implementing servers and services, the dependent evaluation is of computational contexts with shared state and environment.

These types and methods are used to implement higher-level abstractions for server, service, and interface development. See, for example, [pure-html](/packages/pure-html/latest), [pure-svg](/packages/pure-svg/latest), and [pure-events](/packages/pure-events/latest). For a driver that manages instances of these types, see [pure-dom](/packages/pure-dom/latest).

