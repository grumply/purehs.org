----------------
title: Immutability
highlights: []
----------------

Pure relies on the immutability of Haskell terms and expressions to be blazing fast; applying the <a target='_blank' href='https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/base-4.12.0.0/GHC-Exts.html#v:reallyUnsafePtrEquality-35-'>same</a> view function to the same data does not produce any meaningful work - no diffing will be performed. But don't be fooled by nay-sayers, Haskell might be pure, but it still has escape hatches for side-effects, mutation, and other naughty bits.

In the following example, the embedded text will never be deeply inspected for diffing, always short-circuiting on a referrential equality test. If `subview` doesn't change, the referrential equality test on the text won't even need to be performed as the diffing algorithm will short-circuit on `myView someSubview`.

# Code

```haskell
myView :: View -> View
myView subview = 
  Div <||> 
    [ subview
    , "Some immutable text."
    ]
```
