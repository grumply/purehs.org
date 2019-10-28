# pure-cond

This package implements a class for representing and comparing for low-information or failure values.

This library should eventually merge with [pure-default](/doc/pure-default) to avoid aliasing of `Default` and `Cond` values.

One of the key differences between `Default` and `Cond`, and the reason for the split between the two, is that `Cond` is incapable of comparing non-representational types, like `a -> b` or `IO a`. `Cond` works on deconstructable values, while `Default` works with deconstructable values and functions.

## Pure.Data.Cond

`Pure.Data.Cond` exports the class `Cond` with a derivable implementation for types that implement `Generic`.

Many of the instances that `Pure.Data.Cond` exports are arbitrary. That is, there are no laws dictacting what a correct `Cond` instance should look like, or how it should behave.

A good rule of thumb is that the instance should include as little information as possible. Consider these two possible instances for `Maybe`.

```haskell
instance Cond (Maybe a) where
  nil = Nothing
  isNil = isNothing

instance (Cond a) => Default (Maybe a) where
  nil = Just nil
  isNil (Just a) = isNil a
  isNil _ = False
```

The one exported from this library is the first, because it exposes less information and is more generic. It tends to correspond better with nil-oriented conditionals.

### class Cond

The `Cond` class defines a class of defaultable or failure types, depending on the instance, with a derivable instance for generic types.

```haskell
class Cond a where
  nil :: a
  default nil :: (Generic a, GCond (Rep a)) => a
  nil = to gnil

  isNil :: a -> Bool
  default isNil :: (Generic a, GCond (Rep a)) => a -> Bool
  isNil = gisNil . from

  notNil :: a -> Bool
  notNil = not . isNil
```

There is an instance for `Bool`.

```haskell
instance Cond Bool where
  nil = False
  isNil = not
```

There is an instance for `SomeException` where all exceptions are considered nil, but construction of an exception relies on a custom exception type, `Nil`.

```haskell
data Nil = Nil deriving (Show)
instance Exception Nil

instance Cond SomeException where
  nil = toException Nil
  isNil _ = True
```

There are instances for tuples of `Cond` values up to 7-tuples.

```haskell
instance (Cond a, Cond b) => Cond (a,b) where
  nil = (nil,nil)
  isNil (a,b) = isNil a && isNil b
```

There are instances for equatable numeric types.

```haskell
instance {-# OVERLAPPABLE #-} (Num n, Eq n) => Cond n where
  nil = fromInteger 0
  isNil = (== nil)
```  

There is an instance for `Txt`.

```haskell
instance Cond Txt where
  nil = ""
  isNil = Pure.Data.Txt.null
```

There are instances for wrapped `Maybe`, `Bool`, and `Num` wrapped monoids.

```haskell
instance Cond Any where
  nil = Any False
  isNil (Any a) = not a

instance Cond All where
  nil = All False
  isNil (All a) = not a

instance Cond (Last a) where
  nil = Last Nothing
  isNil (Last Nothing) = True
  isNil _ = False

instance Cond (First a) where
  nil = First Nothing
  isNil (First Nothing) = True
  isNil _ = False

instance (Cond a) => Cond (Sum a) where
  nil = Sum nil
  isNil (Sum a) = isNil a

instance (Cond a) => Cond (Product a) where
  nil = Product nil
  isNil (Product a) = isNil a

instance (Cond a) => Cond (Const a b) where
  nil = Const nil
  isNil (Const a) = isNil a
```

There is a special instance for `nil :: Ordering` that differs from `def :: Ordering`. While `nil == LT`, `def == EQ`.

```haskell
instance Cond Ordering where
  nil = LT
  isNil LT = True
  isNil _ = False
```

There is an instance for `Dual` that relies on the wrapped value being an instance of `Cond`.

```haskell
instance (Cond a) => Cond (Dual a) where
  nil = Dual nil
  isNil (Dual d) = isNil d
```

There are instances for many container types that use the empty container as a nil value.

```haskell
instance Cond (Set.Set a) where
  nil = Set.empty
  isNil = Set.null

instance Cond (Seq.Seq a) where
  nil = Seq.empty
  isNil = Seq.null

instance Cond (Map.Map a b) where
  nil = Map.empty
  isNil = Map.null

instance Cond (IntMap.IntMap a) where
  nil = IntMap.empty
  isNil = IntMap.null

instance Cond IntSet.IntSet where
  nil = IntSet.empty
  isNil = IntSet.null

instance {-# OVERLAPPABLE #-} Cond (HM.HashMap a b) where
  nil = HM.empty
  isNil = HM.null

instance Cond (Vector.Vector a) where
  nil = Vector.empty
  isNil = Vector.null
```

There is special support for `Value` and `Obj` to work on both GHCJS and GHC.

```haskell
#ifdef __GHCJS__

instance Cond Obj where
  nil = mempty
  isNil = Prelude.null . objectAssocs

instance Cond Value where
  nil = nullValue
  isNil = (== nullValue)

#else

instance Cond Obj where
  nil = mempty
  isNil = (== mempty)

instance Cond Value where
  nil = Null
  isNil = (== Null)

#endif
```

There is an instance for `View` from [pure-core](/doc/pure-core).

```haskell
instance Cond View where
  nil = NullView Nothing
  isNil (NullView _) = True
  isNil _ = False
```

### ?

The `?` operator is the ternary conditional.

```haskell
infixr 1 ?
(?) :: (Cond x) => x -> a -> a -> a
(?) x t e = if notNil x then t else e
```

It is used with an application operator.

```haskell
value :: Cond a => a -> Double
value a = a ? 1.0 $ 0.0
```

For `(a ~ Bool)`, this would be equivalent to.

```haskell
value :: Bool -> Double
value b = Data.Bool.bool b 0.0 1.0
```

### !?

The `!?` operator is a version of the ternay conditional where the branches are reversed.

```haskell
infixr 1 !?
(!?) :: (Cond x) => x -> a -> a -> a
(!?) x t e = if isNil x then t else e
```

### may

The `may` method is a shorthand for `maybe nil`.

```haskell
may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil
```

### \#

The `#` operator combines `Cond` and `Default` from [pure-default](/doc/pure-default).

```haskell
infixr 6 #
(#) :: (Cond x, Default a) => x -> a -> a
(#) b t = b Pure.Data.Cond.? t $ def
```

### cond

`cond` is a simplified version of `#` that returns the given value if it is non-nil or a default value otherwise. This is usually equivalent to `id`.

```haskell
cond :: (Cond a, Default a) => a -> a
cond a = a # a
```