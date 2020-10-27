module Styles.Responsive 
  ( smallScreens, mediumScreens, largeScreens, hugeScreens
  , (<%>)
  ) where

import Pure.Data.CSS (CSS, atMedia)
import Pure.Data.Styles (pxs)

smallScreens :: Int
smallScreens = 480

-- | mediumScreens are those wider than 768px
mediumScreens :: Int
mediumScreens = 768

-- | largeScreens are those wider than 992px
largeScreens :: Int
largeScreens = 992

-- | hugeScreens are those wider than 1200px
hugeScreens :: Int
hugeScreens = 1200

-- | Construct a media query, given a minimum screen width.
(<%>) :: Int -> CSS a -> CSS () 
(<%>) n = atMedia ("screen and (min-width: " <> pxs n <> ")")
infixr 1 <%>
