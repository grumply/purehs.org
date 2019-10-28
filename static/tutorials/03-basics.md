# Pure Basics

Pure's core type is `View`, a type for declaratively representing nested contexts. The browser's DOM is one such context.

```haskell
pattern Div :: View
```

To evaluate a `View`, it must be injected. In a browser, we might inject into the body of a page.

```haskell
main = inject body Div
```

Attributes and properties are applied as functional transformations.

```haskell
root :: View
root = Div <| Id "root" . Class "container"
```

Views are nestable.

```haskell
logo :: View
logo =
  Div <| Class "logo" |>
    [ Img <| Src "/images/logo.png"
    ]
```

There is special support for raw text.

```haskell
price :: Dollars -> Cents -> View
price dollars cents =
  Span <||>
    [ "$", text dollars, ".", text cents ]
```

`View`s are first class and can be passed around and transformed.

```haskell
colorize :: Txt -> View -> View
colorize color v = v <| Color color

green :: View -> View
green = colorize "green"

goodPrice :: Dollars -> Cents -> View
goodPrice dollars cents =
  green (price dollars cents)
```

Views are also amenable to inspection.

```haskell
isSpan :: View -> Bool
isSpan Span = True
isSpan _ = False
```

See [Components](/tut/components) to learn about state management and reactive contexts in Pure.