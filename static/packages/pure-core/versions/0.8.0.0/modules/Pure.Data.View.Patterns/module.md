## pattern Component

A view pattern for construction of components.

```haskell
pattern Component :: (Ref props state -> Comp props state) -> props -> View
```

<div class="hide">
```haskell
user :: User -> View
user = Component $ \self ->
  def
    { ...
    }
```
</div>

## pattern Null

A null view pattern for conditonal rendering of views.

```haskell
pattern Null :: View
```

<div class="hide">
```haskell
activeUser :: Maybe Username -> View
activeUser Nothing = Null
activeUser (Just u) = {...}
```
</div>

## pattern Raw

A pattern for unsafe construction of an HTML view from a text value.

> WARNING: `Raw` does not sanitize its input!

```haskell
pattern Raw :: Txt -> View
```

<div class="hide">
```haskell
rawH1 = Raw "<h1>Raw H1</h1>"
```
</div>

## pattern Keyed

A pattern for converting a non-keyed view to a keyed variant. 

> ### Note
>
> This pattern only works on `HTML` and `SVG` views. 
>
> Use `(|#>)` or `(<||#>)` to add keyed children to a keyed view.

```haskell
pattern Keyed :: View -> View
```

<div class="hide">
```haskell
keyedDiv :: View
keyedDiv = Keyed Div
```
</div>

## pattern Class

A pattern for adding a class to a view. 

> ### Note
>
> This method only works on HTML and SVG nodes and Portals thereof. For non-HTML and non-SVG nodes, it sets nothing.
>
> Empty class strings are filtered out during rendering in [pure-dom](/doc/pure-dom/latest).
>
> ### Warning
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Classes) and `Data.List.elem` for matching.

```haskell
pattern Class :: HasFeatures a => Txt -> a -> a
```

<div class="hide">
```haskell
blue :: View
blue = Div <| Class "blue"
```
</div>

## pattern Classes

A pattern for viewing or overwriting classes on a view.

```haskell
pattern Classes :: HasFeatures a => [Txt] -> a -> a
```

> ### Note
>
> This method only works on HTML and SVG nodes and Portals thereof. For non-HTML and non-SVG nodes, it sets nothing and only views an empty list.
>
> Empty class strings are filtered out during rendering in [pure-dom](/doc/pure-dom/latest), classes with spaces are broken up into multiple classes when rendered.

<div class="hide">
```haskell
container :: HasFeatures a => a -> a
container = Classes ["wrapped","content"]
```
</div>

## pattern Style

A pattern for adding a style to a view.

> ### Note
>
> This method only works on HTML and SVG nodes and Portals thereof. 

> ### Warning
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Styles) and `Data.List.lookup` for matching.

```haskell
pattern Style :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
```haskell
myView = Div <| Style "width" "100%"
```
</div>

## pattern Property

A pattern for adding a property to a view.

> ### Note
>
> This method only works on HTML and SVG nodes. 

> ### Warning
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Properties) and `Data.List.lookup` for matching.

```haskell
pattern Property :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
```haskell
myView val = Input <| Property "value" val
```
</div>

## pattern Attribute

A pattern for adding an attribtue to a view.

> ### Note
>
> Pattern matching with this pattern will throw an error.
>
> Use a combination of [pattern Classes](Pure.Data.View/pattern%20Attributes) and `Data.List.lookup` for matching.

```haskell
pattern Attribute :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
```haskell
myView = Input <| Attribute "id" "myView"
```
</div>

## <|

The `with properties` operator.

Transform a type into a view-able value or apply  a transformation to an already view-able type.

```haskell
infixl 8 <|
(<|) :: ToView b => a -> (a -> b) -> View
```

<div class="hide">

The operator was designed to look like the left side of [<||>](%3C%7C%7C%3E) to allow for between-bracket transformations:

```haskell
Button <| BackgroundColor blue . OnClick (\_ -> print "Clicked") |>
  [ "Button Content" ]
```

In the simplest case, `<|` can apply a transformation to a `View`.

```haskell
Div <| Class "green" . Id "green-div"
```

Or, `<|` can apply a transformation to a value that can be viewed.

```haskell
data Person = Person { name :: Txt, age :: Int }
instance Pure Person where
  view Person {..} = [i|#{name} is #{age} years old.|]

birthday :: Person -> Person
birthday p = p { age = age p + 1 }

sean :: Person
sean = Person "Sean" 32

myView :: View
myView = sean <| birthday
```

Or, `<|` can transform a value into something that can be viewed.

```haskell
mkPerson :: (Txt,Int) -> Person
mkPerson = uncurry Person

myView :: View
myView = ("Sean",32) <| birthday . mkPerson
```

</div>

The `with children` operator.

## |>

Set children on a value supporting [HasChildren](Pure.Data.View.Patterns/class%20HasChildren). Note that unlike other setters, this children setter does not convert the value to a [View](Pure.Data.View/data%20View). This operator has a very high right fixity.

```haskell
infixr 9 |>
(|>) :: (HasChildren a) => (a -> a) -> [View] -> a -> a
```

<div class="hide">
```haskell
Div <| Class "greeting" |>
  [ Span <| Class "hello" |> [ "Hello" ]
  , Span <| Class "world" |> [ ", World!" ]
  ]
```
</div>

## <||>

The `contains` operator.

Specify children on a value supporting [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), and then convert the value to a [View](Pure.Data.View/data%20View).

```haskell
(<||>) :: (ToView a, ToView b, HasChildren a) => a -> [b] -> View
```

<div class="hide">
```haskell
Div <||>
  [ Span <||> [ "Hello" ]
  , Span <||> [ ", World!" ]
  ]
```
</div>

## |#>

The `with keyed children` operator.

Set keyed children on a value supporting [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren). Note that unlike some other setters, this keyed children setter does not convert the value to a [View](Pure.Data.View/data%20View). This operator has a very high right fixity.

```haskell
infixr 9 |#>
(|#>) :: (HasKeyedChildren a) => (a -> a) -> [(Int,View)] -> a -> a
```

<div class="hide">
```haskell
users us =
  Keyed Div <| Class "users" |#>
    [ (hash u, View u)
    | u <- us
    ]
```
</div>

## <||#>

The `contains keyed` operator.

Specify keyed children without applying attributes, properties, listeners, or lifecycle methods. Must be paired with `Keyed` or used on views supporting `HasKeyedChildren`.

```haskell
(<||#>) :: (ToView a, HasKeyedChildren a) => a -> [(Int,View)] -> View
```

<div class="hide">
```haskell
users us =
  Keyed Div <||#>
    [ (hash u, View u)
    | u <- us
    ]
```
</div>

## text

Inject a textual value into a `View`.

```haskell
text :: IsTxt a => a -> View
```

## txt

A specialized version of `text` for `Txt`.

```haskell
txt :: Txt -> View
```
