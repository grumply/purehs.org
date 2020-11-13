## class Theme

The core theming class, where styles associated with a theme's type are defined.

```haskell
class Theme t where
  theme :: Txt -> CSS ()
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Themed @Green

data Green
instance Theme Green where
  theme c = do
    is c do 
      width  =: 20px
      height =: 20px
      border =* [1px,solid,green]
</pre>
</div>

## pattern Themed

Add a theme to an element or inspect an element for a theme when pattern matching.

```haskell
pattern Themed :: forall t b. (HasFeatures b, Theme t) => b -> b
```

## subtheme

Construct the class associated with a theme. Requires a type application.

```haskell
subtheme :: Theme t => Txt
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Themed @Container |>
    [ Div <| Themed @Green ]

data Green
instance Theme Green

data Container
instance Theme Container where
  theme c =
    is c do
      padding =: 10px
      has (subtheme @Green) do
        width  =: 20px
        height =: 20px
        border =* [1px,solid,green]
</pre>
</div>

## at

Construct a CSS selector from a theme.

```haskell
at :: forall t a. Theme t => CSS a -> CSS ()
at = is (subtheme @t)
```

## within

Wrap a selector scope with a theme.

```haskell
within :: forall t a. Theme t => CSS a -> CSS ()
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Themed @Outer |>
    [ Div <| Themed @Inner ]

data Outer
instance Theme Outer where
theme c =
  is c do
    border =* [1px,solid,green]

data Inner
instance Theme Inner where
theme c =
  is c do
    width  =: 20px
    height =: 20px
    border =* [1px,solid,red]
    within @Outer do
      padding =: 20px
</pre>
</div>
