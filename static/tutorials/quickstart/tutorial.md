## [Try Pure.hs Live](http://try.purehs.org)

If you just want to try Pure.hs in your browser, check out the [live editor and compiler](http://try.purehs.org).

A few examples:

* [Hello, World!](http://try.purehs.org/1651478635519918182)
* [Counter](http://try.purehs.org/7462221264703780747)
* [Random](http://try.purehs.org/10206365626509647436)
* [Input](http://try.purehs.org/15558941103806201411)

<div class="hide">

Examples included here, but hidden, so that the backend will guarantee they are pre-built and cached.

### Hello, World!
<pre data-try>
import Pure

main = inject body "Hello, World!"
</pre>

### Counter
<pre data-try>
import Pure.Elm

main = inject body counter

newtype Counter = Counter Int deriving (Num)
data Msg = Increment | Decrement

counter = run (App [] [] [] (pure (Counter 0)) update view) ()

update Increment _ c = pure (c + 1)
update Decrement _ c = pure (c - 1)

view _ (Counter c) =
  Div <||>
    [ Button <| OnClick (\_ -> command Decrement) |> [ "-" ]
    , txt c
    , Button <| OnClick (\_ -> command Increment) |> [ "+" ]
    ]
</pre>


### Random
<pre data-try>
import Pure.Elm
import Pure.Random

main = do
  s <- newSeed
  inject body (die s)

data Model = Model Seed (Maybe Int)
data Msg = Roll

die s = run (App [] [] [] (pure (Model s Nothing)) update view) ()

update Roll _ (Model s _) =
  let (s',x) = generate (uniformR 1 6) s
  in pure (Model s' (Just x))

view _ (Model _ x) =
  Div <||>
    [ Button <| OnClick (\_ -> command Roll) |> [ "Roll" ]
    , maybe Null txt x
    ]
</pre>

### Input
<pre data-try>
import Pure.Elm hiding (Name)

main = inject body input

newtype Name = Name Txt deriving (ToTxt,FromTxt,Eq,Semigroup)

data Msg = SetName Name

input = run (App [] [] [] (pure (Name "")) update view) ()

update (SetName nm) _ _ = pure nm

view _ nm =
  Div <||>
    [ Input <| OnInput (withInput (command . SetName)) 
             . Placeholder "What's your name?"
    , if nm == (fromTxt "") 
      then Null 
      else txt ("Hello, " <> nm)
    ]
</pre>
</div>

## [Install](./install)

The [Install Tutorial](./install) will walk you through setup and installation of nix, cachix, and the pure-platform. In just a few minutes you can be running Pure.hs on macOS or Linux.

<div class="more">
[Read More >](./install)
</div>

## [Visual Studio Code IDE](./vscode)

The [VSCode Tutorial](./vscode) will help you install a full development IDE for *Visual Studio Code* with the *Haskell Language Server* extension. You'll be ready to develop in Pure.hs with an LSP-based IDE supporting type and documentation on hover, error reporting, and auto-completion.

<div class="more">
[Read More >](./vscode)
</div>

## [Vim IDE](./vim)

The [Vim Tutorial](./vim) will get you on your feet with a full development IDE for neovim with the *coc.nvim* plugin. You'll be ready to develop in Pure.hs with an LSP-based IDE supporting type and documentation on hover, error reporting, and auto-completion.

<div class="more">
[Read More >](./vim)
</div>

## [5-Minute Series](./5-minute)

The [5-Minute Series](./5-minute) takes a tour through the very basics of user-interface design, state management, application design, and server and API development in easy-to-digest 5-minute chunks.

<div class="more">
[Read More >](./5-minute)
</div>