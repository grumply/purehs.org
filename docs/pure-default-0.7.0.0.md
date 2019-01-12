# pure-default

This package implements a class for default values with a set of supplied instances.

For a similarly defined class of equatable `nil` values for use in conditionals, see [pure-cond](/doc/pure-cond/0.7.0.0).

## Pure.Data.Default

`Pure.Data.Default` exports the class `Default` with a default implementation for types that implement `Generic`.

```haskell
class Default a where
  def :: a
```

There are default instances for: 
 
 * Tuples of types with defaultable values
 * Integral and floating types
 * Functions that return defaultable values `Default r => a -> r` as `const def` that is overlapped by
 * Functions of type `a -> a` as `id`
 * IO actions that return defaultable values as `return def`
 * `Bool => False`
 * `[a] => []`
 * `Ordering => Eq`
 * `Maybe => Nothing`
 * wrapped `Monoid`s that correspond with their `mempty` implementations: `Any`, `All`, `First`, `Sum`, `Product`, `Last`, `Endo`
 * `Const => Const def`

Default can be derived for records of the above, or user-defined, defaultable types or sum types using lexicographic order.

As an example, a generically derived `Default` for this type:

```haskell
data SomeSumType 
  = A { a1 :: String
      , a2 :: String -> String
      , a3 :: () -> Int
      , a4 :: Int
      , a5 :: Double
      , a6 :: (SomeSumType,SomeSumType)
      } 
  | B Int
  deriving Generic
```

Is this instance:

```haskell
instance Default SomeSumType where
  def = A def def def def def def
```

Which is equivalent to:

```haskell
def :: SomeSumType
def = A [] id (const 0) 0 0.0 (def,def)
```
