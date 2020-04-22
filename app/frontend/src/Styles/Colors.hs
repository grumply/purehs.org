{-# language NoMonomorphismRestriction #-}
module Styles.Colors where

import Pure.Elm

type Tones = Double -> Txt
type Color = Txt

pureBlue, pureLavender, pureWhite, pureGreen, pureOrange, pureRed :: Tones
pureBlue p = hsl(215,35.14,p)
pureLavender p = hsl(250,49,p)
pureWhite p = hsl(210,2,p)
pureGreen p = hsl(139,68,p)
pureOrange p = hsl(29,90,p)
pureRed p = hsl(6,87,p)

darkBlue, baseBlue, lightBlue, baseLavender, darkLavender, 
  lightLavender, deepLavender,baseWhite, baseGray, lightGray, 
  mediumGray, darkGray,baseGreen, lightGreen, darkGreen, 
  blueHighlight, mono1, mono2, mono3, cyan_, blue_, purple_,
  green_, red1, red2, orange1, orange2, selection :: Color
darkBlue = pureBlue 20
baseBlue = pureBlue 40
lightBlue = pureBlue 90
baseLavender = pureLavender 69
darkLavender = pureLavender 49
lightLavender = pureLavender 89
deepLavender = pureLavender 14
baseWhite = pureWhite 99
baseGray = pureWhite 70
lightGray = pureWhite 90
mediumGray = pureWhite 45
darkGray = pureWhite 20
baseGreen = pureGreen 45
lightGreen = pureGreen 65
darkGreen = pureGreen 25
blueHighlight = rgb(58,173,175)
mono1 = hsl(220,14,71)
mono2 = hsl(220,09,55)
mono3 = hsl(220,10,40)
cyan_ = hsl(187,47,55) -- hue1
blue_ = hsl(207,82,66) -- hue2
purple_ = hsl(286,60,67) -- hue3
green_ = hsl( 95,38,62) -- hue4
red1 = hsl(355,65,65) -- hue5
red2 = hsl(  5,48,51) -- hue5-2
orange1 = hsl( 29,54,61) -- hue6
orange2 = hsl( 39,67,69) -- hue6-2
selection = hsl(220,13,10)

