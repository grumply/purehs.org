----------------
title: Minimal App
highlights: []
----------------

The minimal `Hello, World!` application.

# Code

```haskell
import Pure.App
import Pure.View

main = run $ App "myapp" return def def (dispatch ()) (const . return . partial $ simple "Home" (fromTxt "Hello, World!"))
```

```html
<!DOCTYPE html>
<html>
  <head></head>
  <body></body>
  <script language="javascript" src="/all.js" defer></script>
</html>
```
