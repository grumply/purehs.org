## Control Flow

Using Pure components, it is possible to implement view generation via monadic control flow. This is especially useful for forms with complex behaviors. Below is a counter using state supplied by an implicit component via `runPureWithIO`.

```haskell
import Pure
import qualified Control.Monad.State.Class as State

counter :: View
counter = runPureWithIO (0 :: Int) $ \ref -> do
  -- Note that `runPureWithIO` just specifies the 
  -- base of the monad stack as IO and gives acess 
  -- to the `ref` for modification within views. 
  -- There's also a `runPureWith` for arbitrary 
  -- monad transformers and `runPure` for when you
  -- don't need to update the state from inside the
  -- view.

  liftIO $ putStrLn $ concat $ 
    [ "I'm in your monadic view generator; "
    , "you'll see me every time you update my state."
    ]

  n <- State.get
  
  let 

    button f cs = 
      Button <| OnClick (\_ -> modifyWith ref f) |> cs

  Div =<||>
    [ button pred [ "-1" ]
    , text n
    , button succ [ "+1" ]
    ]
    
main = inject body counter
```
