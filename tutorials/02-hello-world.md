# Hello World

The simplest application is a single line:

```haskell
import Pure.App
import Pure.View

main = run $ App "myapp" return def def (dispatch ()) (const . return . partial $ simple "Home" (fromTxt "Hello, World!"))
```

