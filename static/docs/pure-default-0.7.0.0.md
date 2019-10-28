# pure-default

This package implements a class for default values with a set of supplied reasonable default instances.

For a similarly defined class of equatable `nil` values for use in conditionals, see [pure-cond](/doc/pure-cond/0.7.0.0).

## Pure.Data.Default

`Pure.Data.Default` exports the class `Default` with a derivable implementation for types that implement `Generic`.

Many of the instances that `Pure.Data.Default` exports are arbitrary. That is, there are no laws dictacting what a correct `Default` instance should look like, or how it should behave.

A good rule of thumb is that the instance should include as little information as possible. Consider these two possible instances for `Maybe`.

```haskell
instance Default (Maybe a) where
  def = Nothing

instance Default a => Default (Maybe a) where
  def = Just def
```

The one exported from this library is the first, because it exposes less information and is more generic. It tends to be a better default in configurations, where this library is especially useful.

### class Default

The `Default` class defines a class of defaultable types with a derivable instance for generic types.

```haskell
class Default a where
  def :: a
  default def :: (Generic a,GDefault (Rep a)) => a
  def = gdef
```

There is an instance for `Maybe` values.

```haskell
instance Default (Maybe a) where
  def = Nothing
```

There is an instance for `Txt` values.

```haskell
instance Default Txt where
  def = ""
```

There are instances for defaultable tuples up to 7-tuples.

```haskell
instance (Default a,Default b) => Default (a,b) where
  def = (def,def)
```

There are instances for integral and floating types.

```haskell
instance Default Int where
  def = 0

instance Default Double where
  def = 0.0
```

There is a special set of overlapped instances for functions that return default values.

```haskell
instance {-# OVERLAPPABLE #-} Default r => Default (x -> r) where
  def = const def

instance {-# OVERLAPPING #-} Default (a -> a) where
  def = id
```

There is an instance for `IO`, but no generic monad instance.

```haskell
instance Default a => Default (IO a) where
  def = return def
```

There is an instance for `Bool`.

```haskell
instance Default Bool where
  def = False
```

There is an instance for lists.

```haskell
instance Default [a] where
  def = []
```

There is an instance for `Ordering`.

```haskell
instance Default Ordering where
  def = EQ
```

There are instances for wrapped `Bool`, `Num`, and `Maybe` `Monoid`s that correspond with their `memepty` instances.

```haskell
instance Default Any where
  def = Any False

instance Default All where
  def = All True

instance Default (Last a) where
  def = Last Nothing

instance Num a => Default (Sum a) where
  def = Sum 0

instance Num a => Default (Product a) where
  def = Product 1

instance Default (Endo a) where
  def = Endo id
```

There is an instance for `Dual` that defaults on the wrapped type.

```haskell
instance Default a => Default (Dual a) where
  def = Dual def
```

There is an instance for `Const` that relies on a default instance for the constant value.

```haskell
instance Default a => Default (Const a b) where
  def = Const def
```

Default can be derived for records of the above, or user-defined, defaultable types. For sum types, the generic deriving facility uses lexicographic order.

As an example, a generically derived `Default` for this type.

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

Results in the following instance.

```haskell
instance Default SomeSumType where
  def = A def def def def def def
```

Which is equivalent to this default `SomeSumType`.

```haskell
def :: SomeSumType
def = fix $ \sst -> A [] id (const 0) 0 0.0 (sst,sst)
```
