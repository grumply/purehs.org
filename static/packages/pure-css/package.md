This package implements a core DSL to embed dynamically generated CSS into Pure.hs applications.

This package is generally used alongside [pure-styles](/packages/pure-styles/latest) and [pure-theme](/packages/pure-theme/latest) for a convenient styling and theming experience. 

You probably don't need to include this package as a dependency of your project, as it is exported from the meta packages [pure](/packages/pure/latest) and [pure-elm](/packages/pure-elm/latest).

<pre data-try>
import Pure
import GHC.TypeLits
import Data.Proxy

main = inject body $
  Div <| Themed @(Box 20) |>
    [ Div ]

-- Note that type-level parameterization with DataKinds allows 
-- for configuration of themes at use-site.
data Box (padding :: Nat)
instance KnownNat padding 
  => Theme (Box padding) where
  theme c = do
    let pad = natVal (Proxy :: Proxy padding)
    is c do
      
      -- mix-in; just a plain-old function
      centeringContainer

      -- pure-styles implements utilities for the below `80` to 
      -- be of type `Txt -> Txt`
      width   =: 80px
      height  =: 80px
      padding =: fromInteger pad px
      
      -- The `-` here is `Txt -> Txt -> Txt`. A hack, but 
      -- convenient for styling.
      background-color =: red

      -- Type-safety of tags is given by using the tag of a 
      -- particular HTML element.
      child (tag Div) do
        background-color =: hex 0xFFF

        -- Sadly, `%`, as a postfix operator, requires a 
        -- parenthetical surround.
        width            =: (100%)
        height           =: (100%)

-- A mix-in; may be freely parameterized as a usual function for 
-- configurable mix-ins. Since both style blocks and selectors may
-- be freely inter-mixed within the CSS DSL, mixins may contain 
-- styles and selectors. Mix-ins may, as well, include dynamic 
-- evaluation - any necessary computation for generation.
centeringContainer :: CSS _
centeringContainer = do
  display         =: flex
  justify-content =: center
  align-content   =: center

</pre>

As an example, the page header theme from this site:

```haskell
data PageHeader
instance Theme PageHeader where
  theme c =
    is c do
      font-size     =: 18px
      width         =: (100%)
      margin        =* [0,auto]
      height        =: 80px
      margin-bottom =: 30px

      smallScreens <%> do
        display         =: flex
        justify-content =: space-between
        font-size       =: 24px
        margin-bottom   =: 45px

      mediumScreens <%> do
        width =: 720px

      largeScreens <%> do
        width =: 900px

      has (tag Nav) do
        margin-top  =: 30px
        color       =: toTxt black
        font-weight =: 400
        width       =: (100%)

        lastChild do
          font-size       =: 24px
          display         =: flex
          justify-content =: space-around
          margin-right    =: 16px
          margin-top      =: (-30)px

          smallScreens <%> do
            display      =: block
            margin-right =: 0
            width        =: auto
            margin-top   =: 30px

        has (tag A) do
          margin-top   =: 50px
          font-family  =: usevar title
          margin-right =: 5px
          margin-left  =: 5px
          color        =: usevar gray
          white-space  =: nowrap

          firstChild do
            margin-left =: 0

          lastChild do
            margin-right =: 0

          hover do
            color =: usevar green

            visited do
              color =: usevar green

          visited do
            color =: usevar gray
```