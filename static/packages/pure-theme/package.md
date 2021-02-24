This package implements a technique for dynamically generating and managing CSS and theming in Pure.hs applications.

This package is used alongside [pure-styles](/packages/pure-styles/latest) and [pure-css](/packages/pure-css/latest) for a convenient styling and theming experience. 

You probably don't need to include this package as a dependency of your project, as it is exported from the meta packages [pure](/packages/pure/latest) and [pure-elm](/packages/pure-elm/latest).

<pre data-try>
import Pure

main = inject body $
  Div <| Themed @Green

data Green
instance Theme Green where
  theme c = do
    is c do 
      color =: green
</pre>