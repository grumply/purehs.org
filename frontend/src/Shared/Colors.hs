module Shared.Colors where

import Pure

-- way too many colors

-- pureBlue, pureLavender, pureWhite, pureGreen :: Double -> Txt
pureBlue p = hsl(215,35.14,p)
pureLavender p = hsl(250,49,p)
pureWhite p = hsl(210,2,p)
pureGreen p = hsl(139,68,p)

-- darkBlue, baseBlue, lightBlue :: Txt
darkBlue = pureBlue 20
baseBlue = pureBlue 40
lightBlue = pureBlue 90


-- baseLavender, darkLavender, lightLavender :: Txt
baseLavender = pureLavender 69
darkLavender = pureLavender 49
lightLavender = pureLavender 89
deepLavender = pureLavender 14

-- baseWhite, baseGray, lightGray, darkGray :: Txt
baseWhite = pureWhite 99
baseGray = pureWhite 70
lightGray = pureWhite 90
darkGray = pureWhite 20

-- baseGreen, lightGreen, darkGreen :: Txt
baseGreen = pureGreen 45
lightGreen = pureGreen 65
darkGreen = pureGreen 25

-- blueHighlight :: Txt
blueHighlight = rgb(58,173,175)

-- -- pureGreen per = hsl(153,68,per)
-- pureGreen per = hsl(140,26,per)
-- pureGray per = hsl(0,0,per)
-- pureWhite per = hsl(60,25,per)
-- pureOrange, pureRed :: Double -> Txt
pureOrange per = hsl(29,90,per)
pureRed per = hsl(6,87,per)

-- baseWhite = pureWhite 99.3
-- baseOrange = pureOrange 68
-- baseRed = pureRed 63

-- darkGreen = pureGreen 40
-- baseGreen = pureGreen 59
-- lightGreen = pureGreen 80
-- brightGreen = pureGreen 90

-- baseGray = pureGray 33

-- dark = hsl(218,15,15)

-- Code highlighting

-- h,s,l :: Int
h         = 220 -- hue
s         = 13 -- saturation
l         = 18 -- brightness

-- mono1, mono2, mono3 :: Txt
mono1     = hsl(h,14,71)
mono2     = hsl(h,09,55)
mono3     = hsl(h,10,40)

-- cyan_, blue_, purple_, green_, red1, red2, orange1, orange2 :: Txt
cyan_     = hsl(187,47,55) -- hue1
blue_     = hsl(207,82,66) -- hue2
purple_   = hsl(286,60,67) -- hue3
green_    = hsl( 95,38,62) -- hue4
red1      = hsl(355,65,65) -- hue5
red2      = hsl(  5,48,51) -- hue5-2
orange1   = hsl( 29,54,61) -- hue6
orange2   = hsl( 39,67,69) -- hue6-2

-- fg, bg, gutter, guide, accent, selection :: Txt
fg        = mono1
bg        = black -- hsl(220,13,18)
gutter    = hsl(h,14,45)
guide     = hsla(h,14,71,0.15)
accent    = hsl(h,100,66)
selection = hsl(220,13,10)


{-
pureGreen per = hsl(140,31,per)
pureGray per = hsl(0,0,per)
pureWhite per = hsl(54,12,per)
purePeach per = hsl(29,59,per)
pureRed per = hsl(6,68,per)

baseGreen = pureGreen 70
baseGray = pureGray 33
baseWhite = pureWhite 96
basePeach = purePeach 97
baseRed = pureRed 95

darkGreen = pureGreen 40
mediumGreen = pureGreen 60
lightGreen = pureGreen 80
brightGreen = pureGreen 90
-}

