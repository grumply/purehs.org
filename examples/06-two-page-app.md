----------------
title: A Two-Page App
highlights: []
----------------

Pure comes `batteries-included` so you can get up and running quickly. This example is a simple two-page application containing:

* Routing
* A static home page
* A static head controller
* A dynamic `_Greet` controller

# Code

```haskell
import Pure.App
import Pure.View

main = run $ App "MyApp" return (return ()) Nothing routes pages

data RouteR = HomeR | GreetR Txt deriving Eq

routes = do
  path "/greet/:name" $ do
    name <- "name"
    dispatch (GreetR name)
  dispatch HomeR

pages HomeR = pure (page _Head _Home)
pages (GreetR nm) = do
  with _Greet (setName nm)
  pure (page _Head _Greet)

_Head = static "Head" (Head [] [])

_Home = static "Home" (Div [] "Welcome to MyApp")

data GreetState ctx = GreetState { name :: Txt }

_Greet = Controller {..}
  where
    key = "Greet"
    build = return
    prime = return ()
    model = GreetState def
    view (GreetState nm) =
      Span []
        [ "Welcome, ", Text nm ]

setName nm = modifyModel $ \GreetState {..} -> 
  GreetState { name = nm, ..}
```
