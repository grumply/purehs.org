<p class="drop">
This is the fourth part in the [5-Minute Series](./5-minute) of Pure.hs fundamentals. In the last part, we learned how to compose stateful, dynamic views into full applications to build a dynamic multi-view doomsday machine with built-in countdown clock. 
</p>

In this tutorial, we'll learn how to decompose a monolithic application into more manageable pieces in a similar way to what we did with the countdown clock in [part 2](./applications).

## To Decomplect

Our doomsday machine was a success. Now, every 93 years, we can scorch the earth and safely wait out the shroud of death. But we're a bit unsatisfied with our doomsday machine, as it is written. It's too complex for a simple two-view application. Let's simplify.

We'll split out each of the views, `Doom` and `Gloom`, and let them manage themselves. To communicate between them, we'll use a familiar messaging approach, with a spin.

## Once More, With Feeling

First, we'll simplify our root model. Gloom will now manage it's own state, and our root-level application will simply switch between the two states.

```haskell
data Model = Doom | Gloom
type Msg   = Model

app = run (App [] [] [] Doom update view) ()
  where
    update msg _ _ = pure msg

    view :: Elm Msg => () -> Model -> View
    view _ Doom  = doom  (command Gloom)
    view _ Gloom = gloom (command Doom)
```

Now it is much easier to reason about the state of our root-level application. We simply specialize the type of message to the model to which we wish to transition.

Note that, since `command Gloom`, and `command Doom`, have been constructed inside `view`, the `Elm Msg` constraint has already been satisfied. This will simplify the types of `doom` and `gloom`.

## Doom, but Not Forever

Doom is simply a button. We've unweaved it from our state management and taken it back to its roots. No more `Elm Msg` constraint. We could have used this approach in any of the previous applications because of its genericity. Simple functional views are often the most generic and reusable.

```haskell
doom :: IO () -> View
doom onClick =
  Button <| OnClick (\_ -> onClick) |>
    [ "Fire Missiles!" ]
```

## Gloom, but Only For a While

Gloom now manages its own state, again.

```haskell
import Control.Concurrent
import Pure.Elm

data Countdown = Countdown Time Time
data Clock = Startup | Tick

gloom :: IO () -> View
gloom = run (App [Startup] [] [] mdl update view)
  where
    mdl = Countdown 0 0

update Startup _ _ = do
  forkIO $ forever $ do
    delay Second
    command Tick
  now <- time
  pure (Countdown (now + 93 * Year) now)

update Tick safe (Countdown end _) = do
  now <- time
  when (now >= end) safe
  pure (Countdown end now)

view _ (Countdown end now) = txt $
  let Years y (Months m (Days d _)) = end - now
      ys = show y ++ " years "
      ms = show m ++ " months "
      ds = show d ++ " days "
  in ys ++ ms ++ ds ++
     "until the doomsday shroud has dissipated."
```

We only have two messages, `Startup` when the gloom begins, and `Tick` for every second that ticks by on the clock. Technically, there is a leak here. See if you can find it and fix it. Don't worry if you don't see it, it's not important.

## Warp and Weft

Now, we put it all back together.

<pre data-try>
import Control.Concurrent
import Control.Monad
import Pure.Elm

main = inject body app

data Model = Doom | Gloom
type Msg   = Model

app = run (App [] [] [] Doom update view) ()
  where
    update msg _ _ = pure msg

    view _ Doom  = doom  (command Gloom)
    view _ Gloom = gloom (command Doom)

doom onClick =
  Button <| OnClick (\_ -> onClick) |>
    [ "Fire Missiles!" ]

data Countdown = Countdown Time Time
data Clock = Startup | Tick

gloom :: IO () -> View
gloom = run (App [Startup] [] [] mdl update view)
 where
  mdl = Countdown 0 0

  update Startup _ _ = do
    forkIO $ forever $ do
      delay Second
      command Tick
    now <- time
    pure (Countdown (now + 93 * Year) now)

  update Tick safe (Countdown end _) = do
    now <- time
    when (now >= end) safe
    pure (Countdown end now)

  view _ (Countdown end now) = txt $
   let Years y (Months m (Days d _)) = end - now
       ys = show y ++ " years "
       ms = show m ++ " months "
       ds = show d ++ " days "
   in ys ++ ms ++ ds ++
      "until the doomsday shroud has dissipated."
</pre>

All of `gloom`'s complexity is now managed within `gloom`. It's a subtle, but important, change. Now each of these three components can exist within their own modules (though we don't have multiple modules in our live view). This ability to push state management down the view hierarchy allows for much more composable, and reusable, components. By encapsulating this complexity, our root `app` didn't need to know anything about what `gloom` or `doom` were doing; we are able to just plug the views in.

There are no more ambiguous cases in the update function, where we must catch unexpected cases. Because we disentangled the state management, it is impossible for a `Tick` message to occur while in the `Doom` state because `doom` and `gloom` are mutually exclusive in our application!

If you're adventurous, try chaing the safe date from 93 years to a few seconds with `5 * Second` or `Seconds 5 0` to see the countdown work successfully. See if you can rewrite `gloom` with the same style of decomposition we used at the end of [part three](./applications).

## What have we learned?

This was a lesson on managing complexity. We have factored the state management and pushed complexity to the leaves of our application. This is an important facility in Pure.hs with a large impact on complexity and why this tutorial was important enough to repeat the implementation of part three with no additional functionality.

We've hit a wall with dynamic functionality: only statically known or generated information can be rendered into the application. It's time to incorporate a persistent connection to a backend server to improve the domain of available content and network client applications. Up next, in the [servers tutorial](./servers), we'll use the abstractions we've learned to implement a backend server and in the [APIs tutorial](./apis) we'll implement a type-safe communication layer.



<div class="prev">
[< Prev](./applications)
</div>

<div class="next">
[Next >](./servers)
</div>