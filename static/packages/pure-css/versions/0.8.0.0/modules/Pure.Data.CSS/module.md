## type CSS

The type of the embedded CSS language.

```haskell
type CSS a
```

## scope

Reify the current selector scope as a [Txt](/packages/pure-txt/latest/Pure.Data.Txt/type%20Txt) value. This is a low-level command that can be used to build more abstracted commands that manipulate the scope via [rescope](Pure.Data.CSS/rescope).

```haskell
scope :: CSS Txt
```

## rawCSS

Embed raw CSS. Best avoided, but exists just-in-case.

```haskell
rawCSS :: Txt -> CSS ()
```

## =:

Style declaration command composed of a property and a value. Returns the value for re-use, if desired.

```haskell
(=:) :: Txt -> Txt -> CSS Txt
```

<div class="note">
It can be desirable to pass `-fno-warn-unusued-do-binds` as a GHC option to avoid the excessive warnings produced by standard CSS code.
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body (Div <| Themed @Green)

data Green
instance Theme Green where
  theme c =
    is c do
      border =* [1px,solid,green]
      height =: 20px
      width  =: 20px
</pre>
</div>

## =*

A convenience combinator that allows for setting multiple values in a style declaration with a list. This is useful for shorthand values, like margin's 2 and 4-parameter alternatives. Returns the concatenated value. 

```haskell
(=*) :: Txt -> [Txt] -> CSS Txt
```

Equivalent to

```haskell
(=*) k vs = k =: Txt.unwords vs
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Themed @Outer |> 
    [ Div <| Themed @Inner ]

data Outer
instance Theme Outer where
  theme c =
    is c do
      padding =: 10px
      border  =* [1px,solid,red]

data Inner
instance Theme Inner where
  theme c =
    is c do
      border =* [1px,solid,green]
      width  =: 10px
      height =: 10px
</pre>
</div>

## =!

A convenience combinator that allows for marking style declarations as important. Returns the important-marked value.

```haskell
(=!) :: Txt -> Txt -> CSS Txt
```

## important

A command wrapper that marks all wrapped style declarations as important. This is more general than that of CSS's important keyword, but should generally be used only to wrap individual declarations!

```haskell
important :: CSS a -> CSS a
```

<div class="note">
See [MDN documentation for CSS selector specificity](https://developer.mozilla.org/en-US/docs/Web/CSS/Specificity) for some alternatives to the heavy-handed `important`.
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".btn" do
    font-weight =: 400

  is ".bold" do
    important do
      font-weight =: 700
</pre>
</div>

## select

The core scoping command. This command is low-level and used to write more particular selectors, like [is](Pure.Data.CSS/is), [has](Pure.Data.CSS/has), [child](Pure.Data.CSS/child), and [next](Pure.Data.CSS/next).

```haskell
select :: Txt -> CSS a -> CSS a
```

## rescope

A low-level command for changing the current scope. Must be used along with [scope](Pure.Data.CSS/scope) to modify the current scope.

```haskell
rescope :: Txt -> CSS a -> CSS a
```

## wrap

A low-level command for constructing CSS3 meta-selectors. This command is used to implement meta-selectors like [atKeyframes](Pure.Data.CSS/atKeyframes) and [atMedia](Pure.Data.CSS/atMedia).

```haskell
wrap :: Txt -> CSS a -> CSS a
```

## defvar

A simple [Txt](Pure.Data.Txt/type%20Txt) transformer that turns a property into a CSS variable. Must be used on the left side of a declaration, like [=:](Pure.Data.CSS/%3D%3A). The defined CSS variable can be used with [usevar](Pure.Data.CSS/usevar) as long as the variable is defined in a scope that is ancestral to the use-site.

```haskell
defvar :: Txt -> Txt
```

<div class="note">
See the [MDN documentation for var()](https://developer.mozilla.org/en-US/docs/Web/CSS/var()) for a better understanding of `defvar` and `usevar`.
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Themed @CustomRoot |> 
    [ Div <| Themed @Green ]
    
data CustomRoot
instance Theme CustomRoot where
  theme c =
    is c do
      defvar green =: rgb(12,200,12)

data Green
instance Theme Green where
  theme c =
    is c do
      background-color =: usevar green
      width            =: 20px
      height           =: 20px
</pre>
</div>

## usevar

A simple [Txt](Pure.Data.Txt/type%20Txt) transformer that turns a value into a CSS variable use. Must be used on the right side of a declaration, like [=:](Pure.Data.CSS/%3D%3A). The variable must be defined in a scope that is ancestral to the use-site - see [defvar](Pure.Data.CSS/defvar).

```haskell
usevar :: Txt -> Txt
```

<div class="note">
See the [MDN documentation for var()](https://developer.mozilla.org/en-US/docs/Web/CSS/var()) for a better understanding of `defvar` and `usevar`.
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $
  Div <| Themed @CustomRoot |> 
    [ Div <| Themed @Green ]
    
data CustomRoot
instance Theme CustomRoot where
  theme c =
    is c do
      defvar green =: rgb(12,200,12)

data Green
instance Theme Green where
  theme c =
    is c do
      background-color =: usevar green
      width            =: 20px
      height           =: 20px
</pre>
</div>

## stylesheet

The CSS renderer. Transforms the CSS DSL into it's standard textual counterpart. 

```haskell
stylesheet :: CSS a -> Txt
```

<div class="note">
If you're using [pure-theme](/packages/pure-theme/latest), you won't need to use `stylesheet`, as `pure-theme` will manage stylesheet rendering automatically and on-demand with theme usage.
</div>

## css

A method of embedding CSS into a style tag, by way of the [stylesheet](Pure.Data.CSS/stylesheet) renderer.

```haskell
css :: CSS a -> View
```

<div class="note">
See [pure-theme](/packages/pure-theme/latest) for a convenient approach to automatically manage css.
</div>

## css'

A more general version of [css](Pure.Data.CSS/css) that allows scoping the style tag. This method is currently not useful as most browsers do not support scoping. This is a shame, because scoping of styles in the way intended by the scoped attribute would be extraordinarily convenient for component-based design.
See [here](https://stackoverflow.com/a/45692033) for a description of the history of the attribute.

```haskell
css' :: Bool -> CSS a -> View
```

## is

The direct selector command. Equivalent to juxtaposition in CSS. Extends the current selector.

```haskell
is :: Txt -> CSS a -> CSS ()
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is (tag Button) do
    is ".green" do
      background-color =: green

  -- This is equivalent to above
  is ".btn" . is ".green" $ do
    background-color =: green
</pre>

`is` is equivalent to juxtaposition in CSS. At most one type of `tag` selector plus multiple classes can be combined by nesting `is` to target an element that satisfies all selectors. 

<div class="note">
Only a `tag` element selector of a single type can be used in a chain of `is` combinators.

That is, as in standard CSS, the following can never be satisfied:

```haskell
invalid = 
  is (tag Span) do
    is (tag Div) do
      ...
```
</div>
</div>

## child

The direct descendant selector combinator command. Equivalent to the `>` combinator in CSS. Selects all matching children.

```haskell
child :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for the child combinator](https://developer.mozilla.org/en-US/docs/Web/CSS/Child_combinator).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".btn" do
    child ".name" do
      color =: green
</pre>
</div>

## has

The descendent selector combinator command. Equivalent to the space ` ` combinator in CSS. Selects all matching descendants.

```haskell
has :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for the descendant combinator](https://developer.mozilla.org/en-US/docs/Web/CSS/Descendant_combinator).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".btn" do
    has ".green" do
      background-color =: green

  -- This is equivalent to above
  is ".btn" . has ".green" $ do
      background-color =: green
</pre>
</div>

## next

The adjacent sibling selector combinator command. Equivalent to the `+` combinator in CSS. Selects the next matching sibling.

```haskell
next :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for the descendant combinator](https://developer.mozilla.org/en-US/docs/Web/CSS/Adjacent_sibling_combinator).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".title" do
    next ".description" do
      color =: hex 0x333
</pre>
</div>

## nexts

The general sibling selector combinator command. Equilvaent to the `~` combinator in CSS. Selects all matching subsequent siblings.

```haskell
nexts :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for the general sibling combinator](https://developer.mozilla.org/en-US/docs/Web/CSS/General_sibling_combinator).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".title" do
    nexts ".para" do
      color =: hex 0x333
</pre>
</div>

## or

The alaternative grouping selector combinator command. Allows for pairing the same style declarations with two separate selectors. In standard CSS, this is a comma.

<div class="warn">
It is important to keep in mind that scope is not inherited by the alternate selector. If you want to inherit the scope for the alternative, see [using](Pure.Data.CSS/using).
</div> 

```haskell
or :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS ()
```

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".container" do
    is ".btn" do
      or is ".title" do
        -- Note that this selector is `.container.btn, .title`, 
        -- not `.container.btn, .container.title`
        font-weight =: 700
</pre>

This combinator is not necessary, and it can complicate an otherwise clean stylesheet.

Consider an alternative using Haskell's `let`, an approach I prefer, to accomplish the same styling. The downside is duplication in the rendered stylesheet - a minor issue when using [pure-theme](/packages/pure-theme/latest).

<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  let 
    shared = do
      font-weight =: 700

  is ".container" do
    is ".btn" do
      shared

  is ".title" do
    shared
</pre>
</div>

## using

Apply multiple selectors to a block, maintaining the current scope. Unlike the comma in CSS, this approach duplicates the block for each selector scope. 

```haskell
using :: [CSS a -> CSS b] -> CSS a -> CSS ()
```

<div class="note">
A more general version exists for arbitary traversable structures.

```haskell
use :: (Traversable t, Applicative f) => t (a -> f b) -> a -> f ()
use fs x = for_ fs ($ x)
```
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".container" do
    using [is ".btn",is ".title"] do
      font-weight =: 700
</pre>
</div>

## isn't

The inverse of the `is` selector command. Wraps the given selector in `:not()`.

```haskell
isn't :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for :not()](https://developer.mozilla.org/en-US/docs/Web/CSS/:not) for capabilities and limitations.
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is ".btn" do
    isn't ".small" do
      font-weight =: 700
</pre>

`isn't` can be combined with [scope](Pure.Data.CSS/scope) to implement a combinator that doesn't exist in CSS, `not child of`.

```haskell
without :: Txt -> CSS a -> CSS a
without out css = do
  s <- scope
  isn't (out <> " " <> s) css
```
</div>

## lang

The `:lang()` selector command, allows targeting elements when the language is matched.

```haskell
lang :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for :lang()](https://developer.mozilla.org/en-US/docs/Web/CSS/:lang).
</div>

## nthChild

The `nth-child()` selector command, allows targeting siblings with direct indexing, linear indexing via a function of the form `An+B`, and `odd` or `even` indexing.

```haskell
nthChild :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for nth-child()](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-child).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is (tag Tr) do
    nthChild(1) do
      background =: hex 0x333

    nthChild(even) do
      background =: hex 0xAAA

    nthChild(odd) do
      background =: hex 0xFFF
</pre>
</div>

## nthLastChild

The `nth-last-child()` selector command, allows targeting siblings counting from the end starting from 1 with direct indexing, linear indexing of the form `An+B`, and `odd` and `even` indexing.

```haskell
nthLastChild :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for nth-last-child()](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-child).
</div>

<div class="hide">
<pre data-try>
import Pure

nextToLast = nthLastChild(2)

main = inject body $ txt $ stylesheet do
  is (tag Tr) do

    nextToLast do
      background =: hex 0x444
</pre>
</div>

## nthOfType

The `nth-of-type()` selector command, allows targeting of the siblings matching the type of the direct ancestor selector with direct indexing, linear indexing via a function of the form `An+B`, and `odd` or `even` indexing.

```haskell
nthOfType :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for nth-of-type()](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-of-type).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is (tag P) do
    nthOfType(even) do
      color =: hex 0x333

    nthOfType(odd) do
      color =: hex 0x666
</pre>
</div>

## nthLastOfType

The `nth-of-type()` selector command, allows targeting of siblings matching the type of the direct ancestor selector, counting from the end, with direct indexing, linear indexing via a function of the form `An+B`, and `odd` or `even` indexing.


```haskell
nthLastOfType :: Txt -> CSS a -> CSS ()
```

<div class="note">
See the [MDN documentation for nth-last-of-type()](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-last-of-type).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is (tag P) do
    nthOfType(even) do
      color =: hex 0x333

    nthOfType(odd) do
      color =: hex 0x666
</pre>
</div>

## pseudo

The pseudo selector combinator `:`. This combinator is used to implement other selectors, like [hover](Pure.Data.CSS/hover), [focus](Pure.Data.CSS/focus), and [visited](Pure.Data.CSS/visited), for example.

```haskell
pseudo :: Txt -> CSS a -> CSS ()
```

It simply extends the selector scope by prepending `:` to the supplied selector.

## attr

The `attr[..]` selector command. This combinator matches elements based on the presence or value of an attribute of an element, like the presence of a `title` attribute, a particular `href` value, or a class containing a string.

```haskell
attr :: Txt -> CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for attr[]](https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors).
</div>

## any

The universal selector combinator command. Equivalent to `*` in CSS. Matches any element.

```haskell
any :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for *](https://developer.mozilla.org/en-US/docs/Web/CSS/Universal_selectors).
</div>

## before

A pseudo element selector command for `::before`. Used to select a pseudo element that is the first child of the current scope.

```haskell
before :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for ::before](https://developer.mozilla.org/en-US/docs/Web/CSS/::before).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is (tag A) do
    before do
      content =: "'❯ '"
</pre>
</div>

## after

A pseudo element selector command for `::after`. Used to select a pseudo element that is the last child of the current scope.

```haskell
after :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for ::after](https://developer.mozilla.org/en-US/docs/Web/CSS/::after).
</div>

<div class="hide">
<pre data-try>
import Pure

main = inject body $ txt $ stylesheet do
  is (tag A) do
    after do
      content =: "' → '"
</pre>
</div>

## active

A pseudo class selector command for `:active`. Used to select elements that are active, like a button during mousedown. Follow the `LVHA`-order of `:link`, `:visited`, `:hover`, `:active`.

```haskell
active :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :active](https://developer.mozilla.org/en-US/docs/Web/CSS/:active).
</div>

## visited

A pseudo class selector command for `:visited`. Used to select links that have been visited. Follow the `LVHA`-order of `:link`, `:visited`, `:hover`, `:active`.

```haskell
visited :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :visited](https://developer.mozilla.org/en-US/docs/Web/CSS/:visited).
</div>

## hover

A pseudo class selector command for `:hover`. Used to select elements that have been moused over or pointed at. Follow the `LVHA`-order of `:link`, `:visited`, `:hover`, `:active`.

```haskell
hover :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :hover](https://developer.mozilla.org/en-US/docs/Web/CSS/:hover).
</div>

## focus

A pseudo class selector command for `:focus`. Used to select elements that have been selected by tabbing or clicking.

```haskell
focus :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :focus](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus).
</div>

## enabled

A pseudo class selector command for `:enabled`. Used to select elements that can be selected, clicked, or typed in. The dual of this selector is [disabled](Pure.Data.CSS/disabled).

```haskell
enabled :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :enabled](https://developer.mozilla.org/en-US/docs/Web/CSS/:enabled).
</div>

## disabled

A pseudo class selector command for `:disabled`. Used to select elements that cannot be selected, clicked, or typed in. The dual of this selector is [enabled](Pure.Data.CSS/enabled).

```haskell
disabled :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :disabled](https://developer.mozilla.org/en-US/docs/Web/CSS/:disabled).
</div>

## link

A pseudo class selector command for `:link`. Used to select links that have not been visited. Will be overridden by [active](Pure.Data.CSS/active), [hover](Pure.Data.CSS/hover), and [visited](Pure.Data.CSS/visited). Follow the `LVHA`-order of `:link`, `:visited`, `:hover`, `:active`.

```haskell
link :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :link](https://developer.mozilla.org/en-US/docs/Web/CSS/:link).
</div>

## empty

A pseudo class selector command for `:empty`. Used to select elements that have no children.

```haskell
empty :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :empty](https://developer.mozilla.org/en-US/docs/Web/CSS/:empty).
</div>


## checked

A pseudo class selector command for `:checked`. Used to select radio, checkbox and option elements that are checked or toggled on.

```haskell
checked :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :checked](https://developer.mozilla.org/en-US/docs/Web/CSS/:checked).
</div>

## valid

A pseudo class selector command for `:valid`. Used to select form elements and inputs whose contents are valid.

```haskell
valid :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :valid](https://developer.mozilla.org/en-US/docs/Web/CSS/:valid).
</div>

## invalid

A pseudo class selector command for `:invalid`. Used to select form elements and inputs whose contents are not valid.

```haskell
invalid :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :invalid](https://developer.mozilla.org/en-US/docs/Web/CSS/:invalid).
</div>

## inRange

A pseudo class selector command for `:in-range`. Used to select inputs that have values within the element's `min` and `max` range bounds.

```haskell
inRange :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :in-range](https://developer.mozilla.org/en-US/docs/Web/CSS/:in-range).
</div>

## outOfRange

A pseudo class selector command for `:out-of-range`. Used to select inputs that have values outside the element's `min` and `max` range bounds.

```haskell
outOfRange :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :out-of-range](https://developer.mozilla.org/en-US/docs/Web/CSS/:out-of-range).
</div>

## optional

A pseudo class selector command for `:optional`. Used to select inputs, selects, or textareas that are not required.

```haskell
optional :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :optional](https://developer.mozilla.org/en-US/docs/Web/CSS/:optional).
</div>

## required

A pseudo class selector command for `:required`. Used to select inputs, selects, or textareas that are required for form submission.

```haskell
required :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :required](https://developer.mozilla.org/en-US/docs/Web/CSS/:required).
</div>

## readOnly

A pseudo class selector command for `:read-only`. Used to select elements that are not user-editable.

```haskell
readOnly :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :read-only](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-only).
</div>

## readWrite

A pseudo class selector command for `:read-write`. Used to select elements that are user-editable.

```haskell
readWrite :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :read-write](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-write).
</div>

## root

A pseudo class selector command for `:root`. The root element is the root of a document - in HTML this is `<html>`. This is where [defvar](Pure.Data.CSS/defvar) is often used to define global variable configurations inherited by all elements.

```haskell
root :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :root](https://developer.mozilla.org/en-US/docs/Web/CSS/:root).
</div>

## target

A pseudo class selector command for `:target`. Used to select the element that is the target of the id segment of the current url.

```haskell
target :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :target](https://developer.mozilla.org/en-US/docs/Web/CSS/:target).
</div>

## onlyChild

A pseudo class selector command for `:only-child`. Used to select an element without any siblings.

```haskell
onlyChild :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :only-child](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-child).
</div>

## firstChild

A pseudo class selector command for `:first-child`.

```haskell
firstChild :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :first-child](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-child).
</div>

## firstOfType

A pseudo class selector command for `:first-of-type`.

```haskell
firstOfType :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :first-of-type](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-of-type).
</div>

## lastChild

A pseudo class selector command for `:last-child`.

```haskell
lastChild :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :last-child](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-child).
</div>

## lastOfType

A pseudo class selector command for `:last-of-type`.

```haskell
lastOfType :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :last-of-type](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-of-type).
</div>

## firstLetter

A pseudo class selector command for `:first-letter`.

```haskell
firstLetter :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :first-letter](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-letter).
</div>

## firstLine

A pseudo class selector command for `:last-line`.

```haskell
firstLine :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for :first-line](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-line).
</div>

## selection

A pseudo element selector command for `::selection`. Used to select an element that has been highlighted.

```haskell
selection :: CSS a -> CSS a
```

<div class="note">
See the [MDN documentation ::selection](https://developer.mozilla.org/en-US/docs/Web/CSS/::selection).
</div>

## atMedia

A wrapper command for the `@media` rule. Used to specify a set of styles that only apply in the given media.

```haskell
atMedia :: Txt -> CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for @media](https://developer.mozilla.org/en-US/docs/Web/CSS/@media).
</div>

## atPage

A wrapper command for the `@page` rule. Used to specify a set of styles that only apply when the document is printed.

```haskell
atPage :: Txt -> CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for @page](https://developer.mozilla.org/en-US/docs/Web/CSS/@page).
</div>

## atFontFace

A wrapper command for the `@font-face` rule. Used to specify a custom font face that can be used in the document.

```haskell
atFontFace :: Txt -> CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for @font-face](https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face).
</div>

## atKeyframes

A wrapper command for the `@keyframes` rule. Used to define the steps of a custom animation.

```haskell
atKeyFrames :: Txt -> CSS a -> CSS a
```

<div class="note">
See the [MDN documentation for @keyframes](https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes).
</div>
