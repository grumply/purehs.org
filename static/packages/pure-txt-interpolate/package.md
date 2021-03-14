String interpolation with a TemplateHaskell QuasiQuoter designed to work with [pure-txt](/packages/pure-txt/latest).

```haskell
joined :: Name -> Time -> View
joined nm t = [i|#{nm} joined on #{toPrettyDate t}|]
```