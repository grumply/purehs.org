----------------
title: Immutability
highlights: []
----------------

Pure relies on the immutability of Haskell terms and expressions to be blazing fast; applying the same* view function to the same data does not produce any meaningful work - no diffing will be performed. And don't be fooled by nay-sayers, Haskell might be pure, but it still has escape hatches for side-effects, mutation, and other naughty bits.

In the following example, the embedded text will never be deeply inspected for diffing, always short-circuiting on a referrential equality test.

* Where `same` corresponds to referrential equality.

# Code

```haskell
myView :: View -> View
myView subview = 
  Div <||> 
    [ subview
    , "Some immutable text."
    ]
```
