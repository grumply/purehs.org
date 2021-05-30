<p class="drop">
This is the third part in the [5-Minute Series](./5-minute) of Pure.hs fundamentals. In the [first part](./basics), we learned about the `View` type and how to functionally enrich it to build a UI in a modular fashion to build a doomsday machine. In the [second part](./components), we learned about `Components` and how to manage state to create dynamic UIs to build a doomsday shroud dissipation countdown clock.
</p>

In this tutorial, we'll learn how to apply [The Elm Architecture](https://guide.elm-lang.org/architecture/), a design pattern for easing application development, in Pure.hs.

## Time is a flat circle

Well, the doomsday shroud has finally dissipated. I guess it's time to set up the doomsday machine, once again. This time, we'll make a single program that switches between a doomsday machine and a countdown machine to get this all over with, once and for all.

## Doom and Gloom

We'll create an application state of `Doom` or `Gloom`. `Doom` when our only fate is the firing of the doomsday machine, and `Gloom` once it's been fired and we must wait for the doomsday shroud to dissipate.

We'll also need a message type with messages that represents the `Start` of the application, `Fired` missiles, `Ticked` clocks, and `Dissipated` doomsday shrouds.

<pre data-try>
import Pure.Elm hiding (Start)
import Control.Concurrent
import Control.Monad

data Model = Doom  | Gloom { safe :: Time, remaining :: Time }
data Msg   = Start | Fired | Ticked | Dissipated

app :: View
app = run (App [Start] [] [] (pure Doom) update view) ()

update :: Elm Msg => Msg -> () -> Model -> IO Model
update Start _ mdl = do
  forkIO $ forever $ delay Second >> command Ticked
  pure mdl

update Fired _ Doom = do
  now <- time
  let remaining = 93 * Year
      safe = remaining + now
  pure Gloom {..}

update Ticked _ Gloom {..} = do
  now <- time
  let remaining = safe - now
  when (remaining <= 0) (command Dissipated)
  pure Gloom {..}

update Dissipated _ Gloom {} = pure Doom

update _ _ model = pure model

view :: Elm Msg => () -> Model -> View
view _ Doom = doom 
view _ Gloom {..} = gloom remaining

doom :: Elm Msg => View
doom = 
  Button <| OnClick (\_ -> command Fired) |> 
    [ "Fire!" ]

gloom :: Time -> View
gloom (Years y (Months m (Days d _))) = 
  txt $ show y ++ " years "   ++
        show m ++ " months "  ++
        show d ++ " days until the doomsday shroud has dissipated."

main = inject body app
</pre>

<div data-try="3072388050054526045"></div>

## What all is going on here?

An application is started with the `run` command.

```haskell
app = run (App [Start] [] [] (pure Doom) update view) ()
```

### Properties

Just like with components in the previous tutorial in this series, we have a communication channel to send information to the `App`. In fact, our implementation of *The Elm Architecture* is written with components. In this case, though, the information is a static `()`. 

### Messaging

Since *The Elm Architecture* relies on messaging, we have three lists of messages that can be dispatched when:

1. The application is first started (`[Start]` for us).
2. The application receives an update to its properties.
3. The application is to be shut down and removed from the page.

For us, these three are the `[Start] [] []` in our `app`.

```haskell
app = ... (App [Start] [] [] ...) ...
```

### Update

When a message is dispatched to the `App`, the `update` function will handle it. The update function has three parameters:

1. The current message.
2. The current properties (for us, an unchanging `()`).
3. The current state/model.

```haskell
update :: Elm Msg => Msg -> () -> Model -> IO Model
```

When the update runs, it optionally effects some actions in `IO` and updates the model with the message. 

### The `Start` Message

For the `Start` command, the update function starts a clock that sends a `Ticked` command every second, forever.

```haskell
update Start _ mdl = do
  forkIO $ forever $ do
    delay Second 
    command Ticked
  pure mdl
```

Since the clock was started from update, which has an `Elm Msg` constraint, the call to `command` knows exactly where to send the message.

<div class="info">
If you're a haskell native, you'll better understand the `Elm` constraint as an implicit parameter hidden behind a constraint kind. This causes overwriting issues in cases of nesting for which we'll find a couple of satisfactory resolutions later in this series in the server tutorial.
</div>

### The `Fired` Message

When the doomsday machine is activated, a `Fired` message is sent to the `App` and the update function switches the model to `Gloom` and calculates when the doomsday shroud will dissipate to start the countdown clock.

```haskell
update Fired _ Doom = do
  now <- time
  let remaining = 93 * Year
      safe = remaining + now
  pure Gloom {..}
```

### The `Ticked` Message

When a `Ticked` message is received and the model is `Gloom`, the time remaining is updated. If there is no time remaining, a `Dissipated` message is sent. Note that the update function is able to send messages to itself.

```haskell
update Ticked _ Gloom {..} = do
  now <- time
  let remaining = safe - now
  when (remaining <= 0) (command Dissipated)
  pure Gloom {..}
```

We also could have called update directly.

```haskell
update Ticked _ Gloom {..} = do
  now <- time
  let remaining = safe - now
  if remaining <= 0
    then update Dissipated () Gloom {..}
    else pure Gloom {..}
```

Or, simpler still, just return `Doom` and remove the `Dissipated` constructor altogether.

```haskell
update Ticked _ Gloom {..} = do
  now <- time
  let remaining = safe - now
  if remaining <= 0
    then pure Doom
    else pure Gloom {..}
```

### The `Dissipated` Message

When a `Dissipated` message is received and the model is `Gloom`, `update` changes the model to `Doom` and the view will now, once again, show the button.

```haskell
update Dissipated _ Gloom {} = pure Doom
```

### The Fallback Pattern

Finally, the fallback pattern handles all other cases in which we're not interested. This is important because `Ticked` is only important in the `Gloom` state, but is sent every second regardless, even if the current model is `Doom`, and it is possible, though not likely, that a `Dissipated` message will be received when the model is `Doom`.

```haskell
update _ _ model = pure model
```

It is a good practice, and supported by the compiler, to handle every case, even if you're relatively sure they won't or can't happen.

### The Elm Constraint

Note that both the `update` and `view` functions have the ability to dispatch `Msg` commands. This functionality is implied by the `Elm Msg` constraint. If `view` didn't require `Elm Msg`, then `gloom` would not be able to use it. Try it yourself below, where I have removed the constraint from `view`.

<pre data-try>
import Pure.Elm hiding (Start)
import Control.Concurrent
import Control.Monad

data Model = Doom  | Gloom { safe :: Time, remaining :: Time }
data Msg   = Start | Fired | Ticked | Dissipated

app :: View
app = run (App [Start] [] [] (pure Doom) update view) ()

update :: Elm Msg => Msg -> () -> Model -> IO Model
update Start _ mdl = do
  forkIO $ forever $ delay Second >> command Ticked
  pure mdl

update Fired _ Doom = do
  now <- time
  let remaining = 93 * Year
      safe = remaining + now
  pure Gloom {..}

update Ticked _ Gloom {..} = do
  now <- time
  let remaining = safe - now
  when (remaining <= 0) (command Dissipated)
  pure Gloom {..}

update Dissipated _ Gloom {} = pure Doom

update _ _ model = pure model

view :: () -> Model -> View
view _ Doom = doom 
view _ Gloom {..} = gloom remaining

doom :: Elm Msg => View
doom = 
  Button <| OnClick (\_ -> command Fired) |> 
    [ "Fire!" ]

gloom :: Time -> View
gloom (Years y (Months m (Days d _))) = 
  txt $ show y ++ " years "   ++
        show m ++ " months "  ++
        show d ++ " days until the doomsday shroud has dissipated."

main = inject body app
</pre>

## What have we learned?

Using *The Elm Architecture*, we can manage stateful views in a convenient state machine fashion to build complex, multi-view applications.

Next up, we'll decompose our application into multiple `App` parts and use the communication channel to simplify its interactions in the [Composition Tutorial](./composition) and then we'll see how these same application-building tools apply to server and service development in the [Server Tutorial](./servers)


<div class="prev">
[< Prev](./components)
</div>

<div class="next">
[Next >](./composition)
</div>