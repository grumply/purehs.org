# pure-txt

This package exports the `Txt` type, an `IsTxt` constraint for values that are coercible to `Txt`, two conversion classes `ToTxt` and `FromTxt`, as well as a module for generic text manipulation of `IsTxt` values.

```haskell
import Pure.Data.Txt as Txt
import Data.Char (isSpace)

newtype SomeTxtNewtype = SomeTxtNewtype Txt

trim :: IsTxt a => a -> a
trim = Txt.dropWhileEnd isSpace . Txt.dropWhile isSpace

trim (SomeTxtNewtype "   Hello, World!   ") == SomeTxtNewtype "Hello, World!"
```
