## producing

Manage the production of a tagged `Maybe a` value. 

```haskell
producing :: forall tag. IO a -> (Maybe a -> View) -> View
```

<div class="hide">
<div class="info">
It is common to use a producer as the result of a case analysis. In such cases, when the produced value is of the same type across branches, it can be necessary to disambiguate the branches of the case analysis. Take the following code, for example.

<pre data-try>
import Pure hiding (A)
import Pure.Maybe

data Sum = A | B

main = inject body $ flip Component () $ \self ->
  let swap _ = modify_ self $ \_ -> \case
        A -> B
        B -> A
  in def 
    { construct = pure A
    , render = \_ st -> 
      Div <||>
        [ Button <| OnClick swap |> [ "Swap" ]
        , case st of
            A -> producing (pure "A") (maybe Null fromTxt)
            B -> producing (pure "B") (maybe Null fromTxt)
        ]
    }
</pre>

The reconciler cannot determine that the two branches of the case analysis are actually distinct!

There are two ways to avoid this problem. The first is a simple newtype approach.

<pre data-try>
import Pure hiding (A)
import Pure.Maybe

data Sum = A | B
newtype RA = RA Txt
newtype RB = RB Txt

main = inject body $ flip Component () $ \self ->
  let swap _ = modify_ self $ \_ -> \case
        A -> B
        B -> A
  in def 
    { construct = pure A
    , render = \_ st -> 
      Div <||>
        [ Button <| OnClick swap |> [ "Swap" ]
        , case st of
            A -> producing (RA <$> pure "A") (maybe Null (\(RA t) -> fromTxt t))
            B -> producing (RB <$> pure "B") (maybe Null (\(RB t) -> fromTxt t))
        ]
    }
</pre>

The second is [Tagged](/packages/pure-core/0.8.0.0/Pure.Data.View.Patterns/pattern%20Tagged), which is slightly more convenient and permits abstraction and reuse, in some cases, without newtype coercion.

<pre data-try>
import Pure hiding (A)
import Pure.Maybe

data Sum = A | B
data TagA
data TagB

main = inject body $ flip Component () $ \self ->
  let swap _ = modify_ self $ \_ -> \case
        A -> B
        B -> A
  in def 
    { construct = pure A
    , render = \_ st -> 
      let producer k = producing (pure k) (maybe Null fromTxt)
      in Div <||>
        [ Button <| OnClick swap |> [ "Swap" ]
        , case st of
            A -> Tagged @TagA (producer "A")
            B -> Tagged @TagB (producer "B")
        ]
    }
</pre>
</div>
</div>

## producingKeyed

Manage the production of a keyed `Maybe a` value. If the key changes, the value will be re-requested.

```haskell
producingKeyed
  :: (Eq key) 
  => key 
  -> (key -> IO a) 
  -> (key -> Maybe a -> View) 
  -> View
```

<div class="hide">
<div class="info">
It is common to use a producer as the result of a case analysis. In such cases, when the produced value is of the same type across branches, it can be necessary to disambiguate the branches of the case analysis. Take the following code, for example.

<pre data-try>
import Pure hiding (A)
import Pure.Maybe

data Sum = A | B

main = inject body $ flip Component () $ \self ->
  let swap _ = modify_ self $ \_ -> \case
        A -> B
        B -> A
  in def 
    { construct = pure A
    , render = \_ st -> 
      Div <||>
        [ Button <| OnClick swap |> [ "Swap" ]
        , case st of
            A -> producingKeyed () (\_ -> pure "A")
                  (\_ -> maybe Null fromTxt)
            B -> producingKeyed () (\_ -> pure "B") 
                  (\_ -> maybe Null fromTxt)
        ]
    }
</pre>

The reconciler cannot determine that the two branches of the case analysis are actually distinct!

There are two ways to avoid this problem. The first is a simple newtype approach.

<pre data-try>
import Pure hiding (A)
import Pure.Maybe

data Sum = A | B
data RA = RA Txt
data RB = RB Txt

main = inject body $ flip Component () $ \self ->
  let swap _ = modify_ self $ \_ -> \case
        A -> B
        B -> A
  in def 
    { construct = pure A
    , render = \_ st -> 
      Div <||>
        [ Button <| OnClick swap |> [ "Swap" ]
        , case st of
            A -> producingKeyed () (\_ -> RA <$> pure "A") 
                  (\_ -> maybe Null (\(RA t) -> fromTxt t))
            B -> producingKeyed () (\_ -> RB <$> pure "B") 
                  (\_ -> maybe Null (\(RB t) -> fromTxt t))
        ]
    }
</pre>

The second is [Tagged](/packages/pure-core/0.8.0.0/Pure.Data.View.Patterns/pattern%20Tagged), which is slightly more convenient and permits abstraction and reuse, in some cases, without coerce.

<pre data-try>
import Pure hiding (A)
import Pure.Maybe

data Sum = A | B
data TagA
data TagB

main = inject body $ flip Component () $ \self ->
  let swap _ = modify_ self $ \_ -> \case
        A -> B
        B -> A
  in def 
    { construct = pure A
    , render = \_ st -> 
      let p k = producingKeyed () (\_ -> pure k) 
                  (\_ -> maybe Null fromTxt)
      in Div <||>
          [ Button <| OnClick swap |> [ "Swap" ]
          , case st of
              A -> Tagged @TagA (p "A")
              B -> Tagged @TagB (p "B")
          ]
    }
</pre>
</div>
</div>