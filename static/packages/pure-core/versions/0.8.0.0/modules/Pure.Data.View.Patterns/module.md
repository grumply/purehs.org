## pattern Component

A view pattern for construction of components.

```haskell
pattern Component :: (Ref props state -> Comp props state) -> props -> View
```

<div class="hide">
<pre data-try>
import Pure hiding (user)

data User = User { name :: Txt }

bob = User "bob"

main = inject body (user bob)

user :: User -> View
user = Component $ \self ->
  def
    { construct = pure ()
    , render = \User {..} _ -> txt name
    }
</pre>
</div>

## pattern SimpleHTML

A pattern for construction of simple HTML nodes by tag name. This is the approach that [pure-html](/packages/pure-html/latest) uses to construct, for instance, [Div](/packages/pure-html/0.8.0.0/Pure.Data.HTML/pattern%20Div). For most users, this pattern will never be used.


```haskell
pattern Div :: View
pattern Div = SimpleHTML "div"
```

## pattern SimpleSVG

A pattern for construction of simple SVG nodes by tag name. This is the approach that [pure-svg](/packages/pure-svg/latest) uses to construct, for instance, [Path](/packages/pure-svg/0.8.0.0/Pure.Data.SVG/pattern%20Path). For most users, this pattern will never be used.

```haskell
pattern Path :: View
pattern Path = SimpleSVG "path"
```

## pattern Null

A null view pattern for conditonal rendering of views.

```haskell
pattern Null :: View
```

<div class="hide">
<pre data-try>
import Pure

type Username = Txt

activeUser :: Maybe Username -> View
activeUser Nothing = Null
activeUser (Just u) = txt u

main = inject body $
  Div <||>
    [ activeUser Nothing
    , activeUser (Just "Bob")
    ]
</pre>
</div>

## pattern Portal

A pattern for construction of portals, or views that must be rendered out-of-tree. This can be useful when constructing a modal that needs to be embedded in the `<body>`, for instance.

```haskell
pattern Portal :: Element -> View -> View
```

<div class="hide">
<pre data-try>
import Pure
import Data.Coerce

main = inject body $ 
  Div <||>
    [ Portal (coerce body) 
        "In the body."
    ]
</pre>
</div>

## pattern Raw

A pattern for unsafe construction of an HTML view from a text value.

<div class="warn">
`Raw` does not sanitize its input!
</div>

```haskell
pattern Raw :: Txt -> Txt -> View
```

<div class="hide">
<pre data-try>
import Pure

main = inject body (Raw "div" "&lt;h1&gt;Raw H1&lt;/h1&gt;")
</pre>
</div>

## pattern Keyed

A pattern for converting a non-keyed view to a keyed variant of the same view, if possible. 

```haskell
pattern Keyed :: View -> View
```

<div class="hide">
<div class="info">
This pattern only works on `HTML` and `SVG` views. 

Use [|#>](Pure.Data.View.Patterns/%7C%23%3E) or [<||#>](Pure.Data.View.Patterns/%3C%7C%7C%23%3E) to add keyed children to a keyed view.
</div>

<pre data-try>
import Pure

main = inject body $
  Keyed Div <||#>
    [ (i,txt i)
    | i <- [1..10]
    ]
</pre>
</div>

## class HasFeatures

A typeclass abstracting the functions to get, set, and add features to a structure. Some UI abstractions use this class with non-html structures that eventually get rendered to HTML.

```haskell
class HasFeatures a where
  getFeatures :: a -> Features
  setFeatures :: Features -> a -> a
  {-# INLINE addFeatures #-}
  addFeatures :: Features -> a -> a
  addFeatures fs a = setFeatures (getFeatures a <> fs) a
```

## pattern SetFeatures

A pattern for overwriting [Features](Pure.Data.View/data%20Features) on a structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetFeatures :: HasFeatures a => Features -> a -> a
```

## pattern Features

A pattern for viewing or adding [Features](Pure.Data.View/data%20Features) on a structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Features :: HasFeatures a => Features -> a -> a
```

## pattern Class

A pattern for adding a class to a structure that implements the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Class :: HasFeatures a => Txt -> a -> a
```

<div class="hide">
<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [pattern Classes](Pure.Data.View/pattern%20Classes) and `Data.List.elem` for matching.
</div>

<pre data-try>
import Pure hiding (blue)

blue :: View
blue = Div <| Class "blue"

main = inject body blue
</pre>
</div>

## pattern SetClasses

A pattern for overwriting classes on a structure that implements the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetClasses :: HasFeatures a => [Txt] -> a -> a
```

## pattern Classes

A pattern for viewing or adding classes to a structure that implements the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Classes :: HasFeatures a => [Txt] -> a -> a
```

<div class="hide">
<pre data-try>
import Pure

container :: HasFeatures a => a -> a
container = Classes ["wrapped","content"]

main = inject body (container Div) 
</pre>
</div>

## pattern Style

A pattern for adding a style to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Style :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [Styles](Pure.Data.View/pattern%20Styles) and `Data.List.lookup` for matching.
</div>

<pre data-try>
import Pure

main = inject body $
  Div <| Style "width" "40px" 
       . Style "height" "40px"
       . Style "background-color" "blue"
</pre>
</div>

## pattern SetStyles

A pattern for overwriting styles on a structure that implements the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetStyles :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## pattern Styles

A pattern for adding or viewing the styles of any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Styles :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Styles 
    [("width","40px")
    ,("height","40px")
    ,("background-color","blue")
    ]
</pre>
</div>

## pattern SetStyles

A pattern for overwriting the styles of any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetStyles :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

<div class="hide">
<pre data-try>
import Pure

invisible = Div <| Style "opacity" "0"

main = inject body $
  invisible 
    <| SetStyles 
       [("width","40px")
       ,("height","40px")
       ,("background-color","blue")
       ]
</pre>
</div>

## pattern Listener

A pattern for adding a [Listener](Pure.Data.View/data%20Listener) to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Listener :: HasFeatures a => Listener -> a -> a
```

<div class="hide">
<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [pattern Listeners](Pure.Data.View/pattern%20Listeners) and `Data.List.filter` for matching.
</div>
</div>

## pattern SetListeners

A pattern for overwriting [Listeners](Pure.Data.View/data%20Listener) on any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetListeners :: HasFeatures a => [Listener] -> a -> a
```

## pattern Listeners

A pattern for viewing and adding [Listeners](Pure.Data.View/data%20Listener) to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Listeners :: HasFeatures a => [Listener] -> a -> a
```

## pattern Property

A pattern for adding a property to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Property :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
<div class="info">
If you're unsure, you probably want [Attribute](Pure.Data.View.Patterns/pattern%20Attribute) instead of this pattern.
</div>

<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [pattern Properties](Pure.Data.View/pattern%20Properties) and `Data.List.lookup` for matching.
</div>

<pre data-try>
import Pure

main = inject body $ 
  Input <| Property "value" "Default Value"
</pre>
</div>

## pattern SetProperties

A pattern for setting properties on any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetProperties :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## pattern Properties

A pattern for viewing or adding properties to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Properties :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## pattern Attribute

A pattern for adding an attribute to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Attribute :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [pattern Classes](Pure.Data.View/pattern%20Attributes) and `Data.List.lookup` for matching.
</div>

<pre data-try>
import Pure

main = inject body $ 
  Form <||>
    [ Input <| Attribute "id" "myInput"
    , Label <| For "myInput" |> [ "Label for Input" ]
    ]
</pre>
</div>

## pattern SetAttributes

A pattern for setting attributes on any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetAttributes :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## pattern Attributes

A pattern for viewing or adding attributes to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Attributes :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## pattern Lifecycle

A pattern for adding a [Lifecycle](Pure.Data.View/data%20Lifecycle) to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Lifecycle :: HasFeatures a => Lifecycle -> a -> a
```

<div class="hide">
<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [pattern Lifecycles](Pure.Data.View/pattern%20Lifecycles) and `Data.List.filter` for matching.
</div>
</div>

## pattern SetLifecycles

A pattern for overwriting the [Lifecycles](Pure.Data.View/data%20Lifecycle) on any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern SetLifecycles :: HasFeatures a => [Lifecycle] -> a -> a
```

## pattern Lifecycles

A pattern for viewing or adding [Lifecycles](Pure.Data.View/data%20Lifecycle) to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern Lifecycles :: HasFeatures a => [Lifecycle] -> a -> a
```

## pattern WithHost

A pattern for adding a `HostRef` [Lifecycle](Pure.Data.View/data%20Lifecycle) to any structure supporting the typeclass [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasFeatures](Pure.Data.View.Patterns/class%20HasFeatures), as well.

```haskell
pattern WithHost :: HasFeatures a => (Node -> IO ()) -> a -> a
```

## pattern HasXLinks

A typeclass abstracting the functions to get, set, and add XLinks to a structure. Some UI abstractions use this class with non-svg structures that eventually get rendered to SVG.

```haskell
class HasXLinks a where
  getXLinks :: a -> [(Txt,Txt)]
  setXLinks :: [(Txt,Txt)] -> a -> a
  addXLinks :: [(Txt,Txt)] -> a -> a
```

## pattern XLink

A pattern for adding an `XLink` to any structure supporting the typeclass [HasXLinks](Pure.Data.View.Patterns/class%20HasXLinks). Generally, this is used on a SVG [Views](Pure.Data.View/data%20View), but some UI abstractions may implement [HasXLinks](Pure.Data.View.Patterns/class%20HasXLinks), as well.

```haskell
pattern XLink :: HasFeatures a => Txt -> Txt -> a -> a
```

<div class="hide">
<div class="warn">
Pattern matching with this pattern will throw an error.

Use a combination of [pattern XLinks](Pure.Data.View/pattern%20XLinks) and `Data.List.lookup` for matching.
</div>
</div>

## pattern SetXLinks

A pattern for overwriting the `XLinks` on any structure supporting the typeclass [HasXLinks](Pure.Data.View.Patterns/class%20HasXLinks). Generally, this is used on a SVG [Views](Pure.Data.View/data%20View), but some UI abstractions may implement [HasXLinks](Pure.Data.View.Patterns/class%20HasXLinks), as well.

```haskell
pattern SetXLinks :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## pattern XLinks

A pattern for viewing or adding `XLinks` to any structure supporting the typeclass [HasXLinks](Pure.Data.View.Patterns/class%20HasXLinks). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasXLinks](Pure.Data.View.Patterns/class%20HasXLinks), as well.

```haskell
pattern XLinks :: HasFeatures a => [(Txt,Txt)] -> a -> a
```

## <|

The `with properties` operator.

Transform a type into a view-able value or apply a transformation to an already view-able type.

```haskell
infixl 8 <|
(<|) :: ToView b => a -> (a -> b) -> View
```

<div class="hide">

The operator was designed to look like the left side of [<||>](Pure.Data.View.Patterns/%3C%7C%7C%3E) to allow for between-bracket transformations.

<pre data-try>
import Pure

main = inject body $
  Button <| BackgroundColor blue . OnClick (\_ -> print "Clicked") |>
    [ "Button Content" ]
</pre>

In the simplest case, `<|` can apply a transformation to a [View](Pure.Data.View/data%20View).

<pre data-try>
import Pure

main = inject body $
  Div <| Class "green" . Id "green-div"
</pre>

Or, `<|` can apply a transformation to a value that can be viewed.

<pre data-try>
import Pure

data Person = Person { name :: Txt, age :: Int }
instance Pure Person where
  view Person {..} = txt $
    name <> " is " <> toTxt age <> " years old."

birthday :: Person -> Person
birthday p = p { age = age p + 1 }

bob :: Person
bob = Person "Bob" 41

main = inject body $
  bob <| birthday
</pre>

Or, `<|` can transform a value into something that can be viewed.

<pre data-try>
import Pure

data Person = Person { name :: Txt, age :: Int }
instance Pure Person where
  view Person {..} = txt $
    name <> " is " <> toTxt age <> " years old."

birthday :: Person -> Person
birthday p = p { age = age p + 1 }

mkPerson :: (Txt,Int) -> Person
mkPerson = uncurry Person

main = inject body $
  ("Bob",41) <| birthday . mkPerson
</pre>

</div>

## class HasChildren

A typeclass abstracting the functions to get, set, and add children to a structure. Some UI abstractions use this class with non-html structures that eventually get rendered to HTML.

```haskell
class HasChildren a where
  getChildren :: a -> [View]
  setChildren :: [View] -> a -> a
  addChildren :: [View] -> a -> a
```

## pattern SetChildren

A pattern for overwriting the children on any structure supporting the typeclass [HasChildren](Pure.Data.View.Patterns/class%20HasChildren). Generally, this is used on a [Views](Pure.Data.View/data%20View), but some UI abstractions may implement [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), as well.

```haskell
pattern SetChildren :: HasChildren a => [View] -> a -> a
```

## pattern Children

A pattern for viewing or adding children to any structure supporting the typeclass [HasChildren](Pure.Data.View.Patterns/class%20HasChildren). Generally, this is used on a [Views](Pure.Data.View/data%20View), but some UI abstractions may implement [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), as well.

```haskell
pattern Children :: HasChildren a => [View] -> a -> a
```

## |>

The `with children` operator.

Set children on a value supporting [HasChildren](Pure.Data.View.Patterns/class%20HasChildren). Note that unlike other setters, this children setter does not convert the value to a [View](Pure.Data.View/data%20View). This operator has a very high right fixity. Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), as well.

```haskell
infixr 9 |>
(|>) :: (HasChildren a) => (a -> a) -> [View] -> a -> a
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Class "greeting" |>
    [ Span <| Class "hello" |> [ "Hello" ]
    , Span <| Class "world" |> [ ", World!" ]
    ]
</pre>
</div>

## <||>

The `contains` operator.

Specify children on a value supporting [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), and then convert the value to a [View](Pure.Data.View/data%20View). Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasChildren](Pure.Data.View.Patterns/class%20HasChildren), as well.

```haskell
(<||>) :: (ToView a, ToView b, HasChildren a) => a -> [b] -> View
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <||>
    [ Span <||> [ "Hello" ]
    , Span <||> [ ", World!" ]
    ]
</pre>
</div>

## class HasKeyedChildren

A typeclass abstracting the functions to get, set, and add keyed children to a structure. Some UI abstractions use this class with non-html structures that eventually get rendered to HTML.

```haskell
class HasKeyedChildren a where
  getKeyedChildren :: a -> [(Int,View)]
  setKeyedChildren :: [(Int,View)] -> a -> a
  addKeyedChildren :: [(Int,View)] -> a -> a
```

## pattern SetKeyedChildren

A pattern for overwriting the keyed children on any structure supporting the typeclass [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren). Generally, this is used on a keyed [Views](Pure.Data.View/data%20View), but some UI abstractions may implement [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasChildren), as well.

```haskell
pattern SetKeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
```

## pattern KeyedChildren

A pattern for viewing or adding keyed children to any structure supporting the typeclass [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren). Generally, this is used on keyed [Views](Pure.Data.View/data%20View), but some UI abstractions may implement [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren), as well.

```haskell
pattern KeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
```

## |#>

The `with keyed children` operator.

Analogous to [|>](Pure.Data.View.Patterns/%7C%3E), but for [Keyed](Pure.Data.View.Patterns/pattern%20Keyed) nodes.

Set keyed children on a value supporting [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren). Note that unlike some other setters, this keyed children setter does not convert the value to a [View](Pure.Data.View/data%20View). This operator has a very high right fixity. Generally, this is used on a [View](Pure.Data.View/data%20View), but some UI abstractions may implement [HasKeyedChildren](Pure.Data.View.Patterns/class%20HasKeyedChildren), as well.

```haskell
infixr 9 |#>
(|#>) :: (HasKeyedChildren a) => (a -> a) -> [(Int,View)] -> a -> a
```

<div class="hide">
<pre data-try>
{-# LANGUAGE DeriveAnyClass #-}
module Main where
import Pure
import Data.Hashable
import GHC.Generics

data User = User { name :: Txt, age :: Int }
  deriving (Generic,Hashable)

instance Pure User where
  view User {..} = 
    P <||>
      [ txt $ name <> " is " <> toTxt age <> " years old."
      ]

users :: [User]
users =
  [ User "Alice" 42 
  , User "Bob" 42
  ]

main = inject body $
  Keyed Div <| Class "users" |#>
    [ (hash u, view u)
    | u <- users
    ]
</pre>
</div>

## <||#>

The `contains keyed` operator. 

Analogous to [<||>](Pure.Data.View.Patterns/%3C%7C%7C%3E), but for [Keyed](Pure.Data.View.Patterns/pattern%20Keyed) nodes.

Specify keyed children without applying attributes, properties, listeners, or lifecycle methods. Must be paired with `Keyed` or used on views supporting `HasKeyedChildren`.

```haskell
(<||#>) :: (ToView a, HasKeyedChildren a) => a -> [(Int,View)] -> View
```

<div class="hide">
<pre data-try>
{-# LANGUAGE DeriveAnyClass #-}
import Pure
import Data.Hashable
import GHC.Generics

data User = User { name :: Txt, age :: Int }
  deriving (Generic,Hashable)

instance Pure User where
  view User {..} = 
    P <||>
      [ txt $ name <> " is " <> toTxt age <> " years old."
      ]

users :: [User]
users =
  [ User "Alice" 42 
  , User "Bob" 42
  ]

main = inject body $
  Keyed Div <||#>
    [ (hash u, view u)
    | u <- users
    ]
</pre>
</div>

## txt

Construct a view from a value supporting [ToTxt](/packages/pure-txt/0.8.0.0/Pure.Data.Txt/class%20ToTxt).

```haskell
txt :: ToTxt a => a -> View
```

<div class="hide">
<pre data-try>
import Pure

newtype Username = Username Txt
  deriving (ToTxt)

main = inject body (txt (Username "Alice"))
</pre>
</div>