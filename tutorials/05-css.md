# CSS and Themeing

Pure supplies a [DSL](https://en.wikipedia.org/wiki/Domain-specific_language)
for construction of CSS.

```haskell
myStyles :: CSS ()
myStyles = do

  is ".container" $ do

    apply $ 
      backgroundColor =: gray

    has ".box" .> do
      backgroundColor =: white
      border          =: solid <<>> black
```

`is` creates a class selector, while `has` creates a descendant selector.
Because the `has` selector is nested inside the `is` selector, the full selector
is `.container .box`. Direct descendant selectors are available via `child`.

CSS3 is possible, as well:

```haskell
fadeIn = do
  atKeyframes "fadein" $ do
    is "from" .> opacity =: zero
    is "to"   .> opacity =: one
```

If we want to apply these styles to a page, we must inject them into the <head>.
The easiest way to do this is with themeing:

```haskell
data FadesIn = FadesIn

instance Theme FadesIn where
  theme _ c = do
    fadeIn
    is c .> animation =: "fadein 1s ease-in"

pattern FadingDiv = Theme FadesIn Div
```

Any time the `FadingDiv` is used, a component managing the `<head>` checks to
see if the theme `FadesIn` has already been added to the page, otherwise it 
renders and appends the theme. Anything using `FadingDiv` will load its contents
with a fade-in effect. Note the `c` supplied in `theme` is a special class that
is unique to the `FadesIn` type - it probably looks something like 
`FadesIn_1399980863`.


