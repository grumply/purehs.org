{-# language NoMonomorphismRestriction #-}
module Styles.Colors where

import Pure.Elm hiding (Color,brightness)

data Color = Color
  { hue        :: Int
  , saturation :: Double
  , brightness :: Double
  , alpha      :: Double
  }

instance ToTxt Color where
  toTxt Color {..} = hsba(hue,saturation,brightness,alpha)

lavender = Color 250 66 100 1
teal = Color 171 70 100 1
base = Color 210 2 99 1
green = Color 139 97 90 1
blue = Color 238 70 100 1
red = Color 360 70 100 1
orange = Color 18 70 100 1
black = base { brightness = 15 }
gray = base { brightness = 45 }

highlight c = c { saturation = 45 }
faded c = c { saturation = 20, brightness = 95 }
strong c = c { saturation = 80, brightness = 75 }

-- Markdown colors; integrate these more reasonably into the design
mono1 = hsl(220,(14%),(71%))
mono2 = hsl(220,(09%),(55%))
mono3 = hsl(220,(10%),(40%))
cyan_ = hsl(187,(47%),(55%)) -- hue1
blue_ = hsl(207,(82%),(66%)) -- hue2
purple_ = hsl(286,(60%),(67%)) -- hue3
green_ = hsl( 95,(38%),(62%)) -- hue4
red1 = hsl(355,(65%),(65%)) -- hue5
red2 = hsl(  5,(48%),(51%)) -- hue5-2
orange1 = hsl( 29,(54%),(61%)) -- hue6
orange2 = hsl( 39,(67%),(69%)) -- hue6-2
selection = hsl(220,(13%),(10%))

