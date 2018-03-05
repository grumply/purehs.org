----------------
title: React-style Components
highlights: []
----------------

Pure components are much like [React](https://reactjs.org) components; they admit local state and injectable properties. Unlike [React](https://reactjs.org), Pure components are rendered with preemptive multitasking.

# Code

```haskell
data Timer = Timer

instance (VC ctx) =>
  Pure (Renderable Timer) ctx where
    render (Render Timer) =
      Component () $ \self ->
        let
          reset = void $ setState self $ \_ _ -> 0

          mounted = void $ forkIO $ do
            threadDelay 1000000
            void $ setStateIO self $ \() seconds ->
              return (seconds + 1,mounted)

        in
          def
            { construct = return (0 :: Int)
            , mounted   = mounted
            , renderer  = \_ seconds ->
                Div []
                  [ "Seconds: ", Translated seconds
                  , Br [] []
                  , Button [ onClick reset ] "Reset"
                  ]
            }

timer = View (Render Timer)
```
