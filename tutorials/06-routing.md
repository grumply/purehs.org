# Routing

`pure-route` uses component state to enable multi-page applications via switching on
the current route.

```haskell
import Pure
import Pure.Router

data Route
  = HelloR Txt
  | HomeR

router :: Router Route
router = do
  path "/hello/:nm" $
    dispatch =<< HelloR <$> "nm"
  dispatch HomeR

page :: Route -> View
page HomeR =
  Button <| lref "/hello/friend" |> 
    [ "Say: Hello, Friend!" ]
page (HelloR nm) =
  Span <||> 
    [ "Hello, ", text nm, "!" ]

app :: () -> View
app = 
  Component $ \self ->
    def
      { construct = return HomeR
      , mounted = void $ onRoute' (\r -> modify_ self (\_ _ -> r)
      , render = \_ r ->
        Div <||>
          [ Router HomeR router
          , page r
          ]
      }

main = inject body (app ())
```


