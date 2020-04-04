# Pure Basics

Pure was developed to fill the role of modern application design within the Haskell ecosystem. Pure implements a compelling abstraction for the implementation of web servers and services, web clients, and modern desktop applications based on web technologies.

If you know Haskell, Elm, PureScript, or ReactJS, you'll be able to design applications with Pure.

To start with, we'll cover use of Pure for the implementation of browser-based UIs.

## The View

Pure's core type is [View](/doc/pure-core/latest/Pure.Data.View/data%20View), a type for declaratively representing nested contexts. The browser's DOM is one such context.

For instance, a `<div>` is a `View`.

```haskell
Div :: View
```

To evaluate a `View`, it must be [injected](/doc/pure-dom/latest/Pure.DOM/inject) into a context. In a browser, we might inject into the `<body>` of a page.

```haskell
main = inject body Div
```

Attributes and properties are applied as functional transformations `View -> View`. Here we'll apply an `Id` as a functional transformations.

```haskell
root :: View
root = Id "root" Div
```

There is convenient syntax to make views look more like regular html, as well.

```haskell
root :: View
root = Div <| Id "root" . Class "main-content"
```

Views are nestable.

```haskell
logo :: View
logo =
  Div <| Class "logo" |>
    [ Img <| Src "/images/logo.png"
    ]
```

There is special support for text-based `View`s.

```haskell
price :: Dollars -> Cents -> View
price dollars cents =
  Span <||>
    [ "$", text dollars, ".", text cents ]
```

`View`s are first class and can be transformed.

```haskell
colorize :: Txt -> View -> View
colorize color v = v <| Color color

green :: View -> View
green = colorize "green"

goodPrice :: Dollars -> Cents -> View
goodPrice dollars cents =
  green (price dollars cents)
```

`View`s are also amenable to inspection.

```haskell
isSpan :: View -> Bool
isSpan Span = True
isSpan _ = False
```

By combining inspection and transformation, views can be richly decorated after construction.

You've seen how static views are created and transformed, but dynamics are an important part of user interfaces; see [Components](/tut/components) to learn how to enrich views with interactivity and other non-static functionality.

To see how the same abstractions can be used for the implementation of servers and services, see [Servers](/tut/servers).
