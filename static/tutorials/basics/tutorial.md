<p class="drop">
If you already know HTML/CSS and one of [Haskell](https://www.haskell.org), [Elm](https://elm-lang.org), [PureScript](https://www.purescript.org), or [ReactJS](https://reactjs.org), you'll be able to design applications with Pure.hs. If you're new to functional programming, check out [Learn Haskell](http://learnyouahaskell.com) and change your life! If you're new to web development, check out the Codecademy's [Learn HTML](https://www.codecademy.com/learn/learn-html) followed by [Learn CSS](https://www.codecademy.com/learn/learn-css) and, optionally, [Learn JavaScript](https://www.codecademy.com/learn/introduction-to-javascript).
</p>

If you're ready to learn the Pure.hs fundamentals, the best place to start is in applying Pure.hs abstractions to user interfaces, so let's get started.

## The Fundament

Pure.hs has a core type, [View](/packages/pure-core/latest/Pure.Data.View/data%20View), that lets us create HTML, SVG, and textual nodes. Let's create a regular old `<div>`.

```haskell
Div
```

That's it, just a regular old `<div>`. It isn't special - just a data structure; it can be passed around and manipulated before it's turned into something on the page. Let's turn it into a `<span>`.

```haskell
Div { tag = "span" }
```

Now our `<div>` is a `<span>`. We could have just created a `<span>` directly, instead. 

```haskell
Span
```

Pure.hs has primitives for all HTML and SVG node types.

## We'll do it live

To put a `View` on a page, we must [inject](/packages/pure-dom/latest/Pure.DOM/inject) it into a DOM context. Let's put some text inside the `<body>`. Try it live, below.

<pre data-try>
import Pure
main = inject body "Some Text"
</pre>

Note that `body` is not the same as `Body`, because we want a reference to the live `<body>` DOM node, not a Pure.hs `Body` data structure.

If you hit compile, you'll end up with a very simple page.

```html
<html>
  <body>
    Some Text
  </body>
</html>
```

Since our application, `Some Text`, is just a simple *DOM Text Node*, there's nothing for the injector to manage, and the application simply injects the view and completes successfully. We'll see more advanced views in later tutorials that require the injector to do more work.

## This is the War Room!

But a simple *DOM Text Node* isn't very exciting. Let's create our doomsday machine - a big red button that fires missiles!

First, we need a button.

```haskell
missileControlPanel = Button <||> [ "Fire Missiles!" ]
```

The contents of the button have been specified as a list of `View`, i.e. `[View]`, using the [<||>](/packages/pure-core/latest/Pure.Data.View.Patterns/<||>), or *contains*, operator. We'll need some more operators that allow us to make changes to the button if we want it to be big and red and know how to control our missiles.

## Big and Red

We can modify a `View` with some simple transformations if we use the [<|](/packages/pure-core/latest/Pure.Data.View.Patterns/<|), or *with properties*, operator. Let's use it to make our button big.

```haskell
bigButton = 
  missileControlPanel 
    <| Style "width"  "500px" 
     . Style "height" "200px"
```

Notice we are transforming the `missileControlPanel` instead of writing all of that button mess again. Ruthless efficiency. Now, let's make it red.

```haskell
bigRedButton = 
  bigButton 
    <| Style "background-color" "red"
```

All of these text values are really annoying, right? What if we mistype one of them and tell the browser that our button is `"ted"` instead of `"red"`? Let's fix that.

```haskell
betterBigRedButton = 
  missileControlPanel 
    <| Width (500px) 
     . Height (200px) 
     . BackgroundColor red
```

Pure.hs includes a lot of primitive attributes, properties, styles, event handlers, and utility classes to improve handling and readability. That's great and all, but inline styles aren't great for modularity and readability, so let's create a big red button theme.

```haskell
data BigRedButtonT
instance Theme BigRedButtonT where
  theme bigRedButton =
    is bigRedButton do
      width            =: 500px
      height           =: 200px
      background-color =: red
```

Note that `BigRedButtonT` doesn't have any constructors. We're only really interested in the type. Now, we can use our big red button theme to apply big red button styles anywhere we want.

```haskell
bestBigRedButton = 
  missileControlPanel 
    <| Themed @BigRedButtonT 
```

This will end up rendering as a unique class - something like:

```html
<button class="BigRedButtonT_1198099730">Fire Missiles!</button>
```

The class is unique and tells us the the name of the theme. The call to `Themed` will guarantee that the class and associated styles exist in `<head>` for us.

"But," I hear you saying, "where are the missiles!?" Let's finish this doomsday machine!

## We got some flyin' to do

A button that doesn't do anything might as well not be a button at all, and a big red button that does nothing is a sacrilege. 

To handle user interaction, we need an event handler that we can attach to the button. Pure.hs, or [pure-events](/packages/pure-events/latest) specifically, includes the generic [On](/packages/pure-events/latest/Pure.Data.Events/pattern%20On) event handler as well as a bevy of more specialized handlers, like [OnClick](/packages/pure-events/latest/Pure.Data.Events/pattern%20OnClick).

We'll use [OnClick](/packages/pure-events/latest/Pure.Data.Events/pattern%20OnClick) to fire the missiles.

```haskell
realFireMissilesButton = 
  bestBigRedButton 
    <| OnClick (const (print "Firing!"))
```

Okay, so it's not firing real missiles, but what did you expect? An actual doomsday button is not a thing a sane man would build. I'm all out of [cobalt thorium G](https://www.youtube.com/watch?v=aSlf2vB80lo?t=10), anyways.

Note that we discarded the actual click event with `const` and just fired the missiles since we're not interested in any of the event properties. 

Finally, let's put it all together with a minor change to the `stdout` buffering mode to see output immediately in a web console. Try it live, below.

<pre data-try>
import Pure
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  inject body missileControlPanel

data BigRedButtonT
instance Theme BigRedButtonT where
  theme bigRedButton =
    is bigRedButton do
      width            =: 500px
      height           =: 200px
      background-color =: red

missileControlPanel =
  Button <| Themed @BigRedButtonT . OnClick (\_ -> print "Firing!") |>
    [ "Fire Missiles!" ]
</pre>

We've a big red button that doesn't look too great. Maybe you can make it look better?

## What have we learned?

Using first-class views, we can pass around bits and pieces of our UI and enrich them with functionality in a way that lets us build up from pincipal pieces. Using some simple themeing tools, we can richly decorate views in a safe way with near-CSS syntax. Using event handlers we can program user-interaction-reactive functionality into our applications.

Now that we've seen how static views are created and transformed, let's learn about dynamics - an important part of user interfaces. We'll see how to enrich views with state and other dynamic functionality in the [Components Tutorial](./components). Then, we'll learn how to use [The Elm Architecture](https://guide.elm-lang.org/architecture/) to bootstrap a fully integrated application in the [Applications Tutorial](./applications). After that, we'll apply some of the same abstractions to server-side development in the [Servers Tutorial](./servers).

<div class="next">
[Next >](./components)
</div>