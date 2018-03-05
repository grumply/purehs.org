----------------
title: A Pure View
highlights: []
----------------

All rendering in Pure passes through the `Pure` typeclass `render` function. By implementing an instance of `Pure` for your datatype, you can render that data directly into a view. When the data changes, the view updates accordingly. By tracking context, `ctx`, in the data, it's possible to access context-local capabilities through event listeners.

# Code

```haskell
data Greet ctx = Greet Txt

instance Typeable ctx => Pure Greet ctx where
  render (Greet msg) =
    Span [] [ "Hello ", fromTxt msg ]
    
greeting = View (Greet "Sean")
```
