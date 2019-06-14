## Pure Basics

This pure tutorial assumes a basic knowledge of functional programming and types
and a basic knowledge of core webpage technologies like html, css, and js.

-- Move to advanced:
Pure is, at it's core, a simple ADT of computational contexts to be evaluated in
a concurrent and hierarchical fashion.

Pure's core type is a `View`, which can be seen as an abstraction over the 
browser's DOM.

```haskell
pattern Div :: View
pattern Span :: View
```

A `View` is injected into an existing browser node:

```haskell
main = inject body Div
```

To add properties or attributes to a View:

```haskell
root :: View
root = Div <| Id "root"

container :: View
container = Div <| Class "container"
```

Views are nestable:

```haskell
logo :: View
logo = 
  Div <| Class "logo" |> 
    [ Img <| Src "/images/logo.png" 
    ]
```

There is a special support for text:

```haskell
price :: Dollars -> Cents -> View
price dollars cents = 
  Span <||>
    [ "$", text dollars, ".", text cents ]
```

Pure `View`s are first class and can be passed around and transformed:

```haskell
colorize :: Txt -> View -> View
colorize color v = 
  v <| Styles ("color:" <> color)

goodPrice :: Dollars -> Cents -> View
goodPrice dollars cents = 
  colorize "green" (price dollars cents)
```

If we partially apply `colorize` to only a desired color, we get a 
transformation that will change the background color on any view to our desired
color:

```haskell
makeBlue :: View -> View
makeBlue = colorize "blue"
```

To abstract over a `View` and create a data-oriented segment of UI, there exists the
`Pure` class:

```haskell
class Pure v where
  view :: v -> View
```

This `Pure` abstraction often makes it easier to extend a view with
higher-order properties while maintaining a data-oriented design approach, and 
helps keep application state focused.  As an example, consider a simple person 
abstraction:

```haskell
data Person = Person 
  { name :: Txt
  , bio  :: Txt
  }

jerry :: Person
jerry = Person "Jerry Garcia" "An avid guitar player."
```

Supply an instance of `Pure` to specify a default view for any `Person`:

```haskell
instance Pure Person where
  view Person {..} =
    Div <| Class "person" |>
      [ Div <| Class "name" |> [ txt name ]
      , Div <| Class "bio"  |> [ txt bio  ]
      ]
```

Now, `View jerry` will result in:

```html
<div class='person'>
  <div class='name'>Jerry Garcia</div>
  <div class='bio'>An avid guitar player.</div>
</div>
```
