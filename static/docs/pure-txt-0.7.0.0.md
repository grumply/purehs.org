# pure-txt

This package implements the `Txt` type, a view-oriented type for textual values that is performant and compatible with both GHC and GHCJS.

```haskell
import Pure.Data.Txt as Txt
import Data.Char (isSpace)

newtype SomeTxtNewtype = SomeTxtNewtype Txt

trim :: IsTxt a => a -> a
trim = Txt.dropWhileEnd isSpace . Txt.dropWhile isSpace

trim (SomeTxtNewtype "   Hello, World!   ") == SomeTxtNewtype "Hello, World!"
```

## Pure.Data.Txt

`Pure.Data.Txt` generalizes functions from `Data.Text` with the `IsTxt` class. This allows for generic application to newtype wrapped `Txt` values or types with `ToTxt/FromTxt` instances without worrying about coercions and wrappings.

For example

```haskell
Data.Text.dropWhileEnd :: (Char -> Bool) -> Text -> Text
```

gets lifted to

```haskell
Pure.Data.Txt.dropWhileEnd :: IsTxt a => (Char -> Bool) -> a -> a
Pure.Data.Txt.dropWhileEnd = coerce Data.Text.dropWhileEnd
```

The set of functions that are implemented for `Txt` are the intersection of functions from [ghc/text](http://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html) and [ghcjs/jsstring](https://github.com/ghcjs/ghcjs-base/blob/master/Data/JSString.hs), but note that, since `Txt` is a type synonym, those functions that are not in the intersection may still be used if the host system matches - i.e. ghc/text on ghc and ghcjs/jsstring on ghcjs, but that code will not compile with both compilers.

### data Txt

`Txt` is an inter-compatible wrapper around `Text` on GHC and `JSString` on GHCJS. This type is used and treated the same on both GHC and GHCJS, easing inter-compatability of server and client usage of textual types.

```haskell
#ifdef __GHCJS__
type Txt = GHCJS.Types.JSString
#else
type Txt = Data.Text.Text
#endif
```

### type IsTxt

`IsTxt` is a constraint type that specifies that a type is coercible to `Txt`.

```haskell
type IsTxt a = Coercible Txt a
```

### class ToTxt

`ToTxt` specifies that a type is capable of being converted to a `Txt` value.

```haskell
class ToTxt a where
  toTxt :: a -> Txt
  {-# INLINE toTxt #-}
  default toTxt :: Coercible a Txt => a -> Txt
  toTxt = coerce
```

For newtypes around `Txt`, it is possible, though not generally necessary, to derive a `ToTxt` instance.

```haskell
newtype MyTxt = MyTxt Txt
  deriving (ToTxt)
```

It is important to note that `ToTxt.toTxt` can throw exceptions! If you don't know where a value came from, be careful!

These instances of `ToTxt` are unsafe (due to utf-8 decoding being unsafe in Data.Text.Encoding)!

* ByteString
* Lazy ByteString

This is /generally/ a non-issue, as conversion *to* `ByteString` is uncommon.

Safe instances exist as a general convenience for integral and floating and textual types.

### class FromTxt

`FromTxt` specifies that a type is capable of being converted from a `Txt` value.

```haskell
class FromTxt a where
  fromTxt :: Txt -> a
  {-# INLINE fromTxt #-}
  default fromTxt :: Coercible Txt a => Txt -> a
  fromTxt = coerce
```

For newtypes around `Txt`, it is possible, though not generally necessary, to derive a `FromTxt` instance.

```haskell
newtype MyTxt = MyTxt Txt
  deriving (ToTxt)
```

These instances of FromTxt should be *safe* as they are structural, rather than logical, conversions!

* String
* Text
* Lazy Text
* ByteString
* Lazy ByteString

Unsafe instances exist, as a general convenience, for integral and floating types. That is, for example, `fromTxt "a" :: Int` will throw an exception. These unsafe instances are intended to be used in conversion pipelines where there are guarantees about the input w.r.t. safe construction of valid, readable values.

