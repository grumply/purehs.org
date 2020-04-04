module Styles.Responsive where

import Pure.Data.CSS
import Pure.Data.Styles
import Pure.Data.Txt

smallScreens, mediumScreens, largeScreens, hugeScreens :: Int
smallScreens = 480
mediumScreens = 768
largeScreens = 992
hugeScreens = 1200

infixr 1 <#>
(<#>) :: Int -> Styles Txt -> CSS Txt
(<#>) n = (.>) (atMedia ("screen and (min-width: " <> pxs n <> ")"))
