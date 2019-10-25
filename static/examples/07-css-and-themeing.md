## CSS and Themeing

Pure implements a simple DSL in [pure-css](/doc/pure-css/0.7.0.0) for embedded CSS so you **never have to leave Haskell.**

```haskell
yayBorderBox :: CSS
yayBorderBox = do
  is "*" . or is ":after" . or is ":before" .> do
    boxSizing           =: inherit
    "-wekit-box-sizing" =: inherit

  is "html" .> do
    boxSizing =: borderBox
```

This produces the following css:

```css
*, :after, :before {
	box-sizing: inherit;
	-wekit-box-sizing: inherit
}

html {
	box-sizing: border-box
}
```

But if all you have in hand is `yayBorderBox`, you'll need to inject it into the `<head>` of your aplication. So Pure goes one step further with [pure-theme](/doc/pure-theme/0.7.0.0) to allow Pure to manage styles automatically:

```haskell
data PageT = PageT
instance Themeable PageT where
  theme c _ = void $ is c .> do
    margin          =: auto
    fontSize        =: ems 3
    color           =: darkgrey
    backgroundColor =: teal
```

Now, by applying `Theme PageT`, `pure-theme` will automatically append the unique page theme to our page `<head>` once only.

```haskell
myView = Div <| Theme PageT |> [ "This is a PageT-styled view!" ]
```
