## i

A quasiquoter for `Txt` interpolation. Expressions within `#{...}` are interpolated to `Txt` via the `ToTxt` typeclass.

```haskell
example :: Txt -> View
example x = [i|The value of x is: #{x}|]
```

<div class="note">
GHCJS is not especially efficient with TemplateHaskell splices, so excessive use of this interpolator will cause a compilation slowdown. Whether this trade-off is worth it depends on your use-case and development approach, but it is worth keeping the implications in mind.
</div>