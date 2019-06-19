# Pure Basics

Pure's core type is `View`, an abstraction over the browser's DOM.

```haskell
pattern Div :: View
```

To evaluate a `View`, it must be injected:

```haskell
main = inject body Div
```

Attributes and properties are applied as transformations of HTML views:

```haskell
root :: View
root = Div <| Id "root" . Class "container"
```

Views are nestable:

```haskell
logo :: View
logo = 
  Div <| Class "logo" |> 
    [ Img <| Src "/images/logo.png" 
    ]
```

There is special support for text nodes:

```haskell
price :: Dollars -> Cents -> View
price dollars cents = 
  Span <||>
    [ "$", text dollars, ".", text cents ]
```

Pure `View`s are first class and can be passed around and transformed:

```haskell
colorize :: Txt -> View -> View
colorize color v = v <| Color color

green :: View -> View
green = colorize "green" 

goodPrice :: Dollars -> Cents -> View
goodPrice dollars cents = 
  green (price dollars cents)
```

Next, check out the [Components tutorial](/tut/components).
