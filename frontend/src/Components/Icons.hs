module Components.Icons where

import Control.Monad

import Pure.Elm
import Pure.Data.CSS
import Pure.Data.SVG
import Pure.Data.SVG.Properties
import Pure.Router (lref)
import Pure.Theme

import Themes

gitHubLink link t =
  A <| Href link . Target "_blank" . Theme t . Rel "noopener" . Attribute "title" "GitHub" |>
    [ gitHubLogo ]

gitHubLogo =
  Svg <| ViewBox "0 0 24 24" |>
    [ Path <| D "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
    ]

logo simple linked theme =
  let svg True  = simpleLogo
      svg False = regularLogo
  in
    Div <| Theme theme |>
      [ if linked then
          A <| lref "/" |> [ svg simple ]
        else
          svg simple
      ]

simpleLogo =
  Svg <| Theme SimpleLogoT . ViewBox "0 0 50.23 50.48" |>
    [ G <||> [ p ] ]
  where

    p = Path <| Class "letter" . D "M41.14,18.42c0,7-6.6,10.74-10.74,10.74a1.32,1.32,0,0,1-1.33-1.3s0-.06,0-.08a1.25,1.25,0,0,1,1.26-1.2c3.42-.18,8.16-3.18,8.16-8.16,0-4.68-3.66-8-8.7-8a14.23,14.23,0,0,0-7.32,1.92c2.44,4.64,3.44,10.82,3.44,16.82,0,11-3.42,17.28-8.58,17.28-4.42,0-7.42-4-7.42-12.72C9.92,26.1,12.68,17,19,11.64a9.19,9.19,0,0,0-7.12-3.72,1.23,1.23,0,0,1-1.32-1.14V6.72A1.36,1.36,0,0,1,11.86,5.4,11.33,11.33,0,0,1,21,10.2,17.45,17.45,0,0,1,29.8,7.92C36.52,7.92,41.14,12.42,41.14,18.42ZM20.26,13.86c-5.58,5-7.68,13.86-7.68,19.86,0,5.22,1.5,10.38,4.86,10.38,3.12,0,5.7-5.58,5.82-14.4C23.32,24.06,22.48,18.06,20.26,13.86Z"

regularLogo =
  Svg <| Theme RegularLogoT . ViewBox "0 0 157.38 50.58" |>
    [ G <||> [ open, pure, dot, hs, close ] ]
  where

    pure = G <| Class "pure" |> [ p, u, r, e ]
    hs   = G <| Class "ext"  |> [ h, s ]

    path cls d = Path <| Class cls . D d

    bracket = path "bracket"
    letter  = path "letter"
    period  = path "period"
    ext     = path "ext"

    open = bracket "M13.86,47.94a1.32,1.32,0,0,1,1.2,1.38,1.2,1.2,0,0,1-1.44,1.14H9.18c-2.46,0-5.34-.18-6.24-1C1.2,47.76.36,43.5.06,28.2,0,26.76,0,25.08,0,23.4,0,14.58.3,4,1.14,1.62A1.58,1.58,0,0,1,2.64.42H4.92A51.69,51.69,0,0,0,12.54,0h.18A1.26,1.26,0,0,1,14,1.32a1.23,1.23,0,0,1-1.14,1.2,61.55,61.55,0,0,1-9.42.42C3,5.64,2.7,13.92,2.7,23c0,12.48.42,23.22,1.8,24.12,1,.6,2.94.78,4.74.78Z"

    p = letter "M47.22,13.5c0,7-6.6,10.74-10.74,10.74a1.31,1.31,0,0,1-1.32-1.38,1.26,1.26,0,0,1,1.26-1.2c3.42-.18,8.16-3.18,8.16-8.16,0-4.68-3.66-8-8.7-8a14.33,14.33,0,0,0-7.32,1.92C31,12.06,32,18.24,32,24.24c0,11-3.42,17.28-8.58,17.28C19,41.52,16,37.5,16,28.8c0-7.62,2.76-16.68,9.06-22.08A9.21,9.21,0,0,0,17.94,3a1.23,1.23,0,0,1-1.32-1.2A1.37,1.37,0,0,1,17.94.48a11.34,11.34,0,0,1,9.12,4.8A17.56,17.56,0,0,1,35.88,3C42.6,3,47.22,7.5,47.22,13.5ZM26.34,8.94c-5.58,5-7.68,13.86-7.68,19.86,0,5.22,1.5,10.38,4.86,10.38,3.12,0,5.7-5.58,5.82-14.4C29.4,19.14,28.56,13.14,26.34,8.94Z"

    u = letter "M51.24,22.68a50.4,50.4,0,0,0-1.44,10.5c0,2.76.54,5.64,2.52,5.64,3.84,0,7.32-7.8,8.64-17.28a1.38,1.38,0,0,1,1.56-1.14c.6,0,1.08.78,1,1.5s-.42,2.88-.84,4.92a55.87,55.87,0,0,0-1,8.1,5.79,5.79,0,0,0,1.2,4.14,1.36,1.36,0,0,1,.3.9,1.33,1.33,0,0,1-1.32,1.32c-1.26,0-2.76-2.52-2.7-5.88-1.56,3.72-4.14,6-6.78,6-2.16,0-5.16-1.38-5.16-8.22A59,59,0,0,1,48.48,22.5c.12-.54.3-1.5,1.38-1.5a1.4,1.4,0,0,1,1.38,1.38Z"

    r = letter "M70.08,40.14a1.3,1.3,0,0,1-1.32,1.14A1.23,1.23,0,0,1,67.56,40c0-.42,1.68-10.26,1.68-12.24,0-2.64-.54-3.66-1.32-4.44a1.14,1.14,0,0,1-.36-1A1.32,1.32,0,0,1,68.82,21c1.14,0,2.82,2,2.7,5a8,8,0,0,1,7.56-5.76,1.36,1.36,0,0,1,1.44,1.32,1.22,1.22,0,0,1-1.26,1.2c-6.24.36-7.44,7.68-8.82,15.36Z"

    e = letter "M93.54,25c0-1.2-.84-2-2.4-2-3,0-7.26,3.84-7.26,10.26,0,2,.66,5.7,5,5.7a5.5,5.5,0,0,0,4.2-2,1.17,1.17,0,0,1,1-.48,1.25,1.25,0,0,1,1.2,1.38c0,1.14-2.88,3.54-6.54,3.54-4.56,0-7.5-3.06-7.5-8.16,0-7.56,5.28-12.84,10-12.84,3.6,0,5,2.22,5,4.62,0,4.74-5,7.08-8.52,7.08a1.24,1.24,0,0,1-1.32-1.26,1.37,1.37,0,0,1,1.32-1.32C90.54,29.46,93.54,27.72,93.54,25Z"

    dot = period "M100,38.1A1.45,1.45,0,0,1,101.7,37a1.39,1.39,0,0,1,1.26,1.5l-.3,2a1.41,1.41,0,0,1-1.5,1.08,1.31,1.31,0,0,1-1.44-1.5Z"

    h = ext "M108.24,40a25.88,25.88,0,0,1-.42-4.92,86.3,86.3,0,0,1,2-16.8,13.57,13.57,0,0,1-1.62.06c-1.08,0-2.34.06-2.34-1.32a1.15,1.15,0,0,1,1.26-1.2h1.14a13.5,13.5,0,0,0,2.28-.18c1.86-5.94,5.58-12.72,10.2-12.72a3.74,3.74,0,0,1,4.08,4c0,4.86-6.18,9.36-12.18,10.86a104.09,104.09,0,0,0-2.22,12.18c1.8-6,5.46-9.66,8.7-9.66,4.74,0,4.8,6.3,4.8,8.88a42.75,42.75,0,0,1-1.44,10.44,1.34,1.34,0,0,1-1.26,1,1.38,1.38,0,0,1-1.38-1.32c0-.12.06-.24.06-.36a43,43,0,0,0,1.32-9.78c0-4.56-.78-6.12-2.28-6.12-3.9,0-9,10-8.16,16.68A1.18,1.18,0,0,1,109.5,41,1.29,1.29,0,0,1,108.24,40Zm5.22-25c4.68-1.62,8.7-5.1,8.7-8a1.38,1.38,0,0,0-1.5-1.56C118.5,5.34,115.32,9.3,113.46,14.94Z"

    s = ext "M134,33.9a1.05,1.05,0,0,1-1.2,1.14,4.54,4.54,0,0,0-1-.18c-1.14,0-1.44.9-1.44,1.74,0,1.44,1.74,2.88,3.9,2.88s4.44-1.26,4.44-4c0-1.44-.72-3.12-3.42-4.56-1.92-1-4.38-2.64-4.38-5.64,0-2.46,2-5,5.7-5s5,2.58,5,3.6a1.36,1.36,0,0,1-1.38,1.38,1.21,1.21,0,0,1-1.08-.78,2.82,2.82,0,0,0-2.58-1.92,2.72,2.72,0,0,0-3,2.76c0,1.14.54,1.86,2.94,3.3,2.94,1.74,4.92,3.48,4.92,6.54,0,3.72-2.64,6.48-7.2,6.48-3.42,0-6.54-2-6.54-5a4.15,4.15,0,0,1,4.14-4.32C132.9,32.34,134,32.7,134,33.9Z"

    close = bracket "M143.76,47.52c2.7.18,6.06.48,8.1.48a9,9,0,0,0,2.1-.18,67.37,67.37,0,0,1-.36-7.8c0-5.64.36-8.64.6-15.84.24-5.76.48-12.3.48-16.62a34.28,34.28,0,0,0-.18-4.38,3.45,3.45,0,0,0-2-.42,37.33,37.33,0,0,0-4.92.54,23.37,23.37,0,0,1-3,.36,1.3,1.3,0,0,1-1.26-1.38,1.18,1.18,0,0,1,1.26-1.2c.54,0,1.5-.18,2.58-.3A37.51,37.51,0,0,1,152.4.24c5,0,5,2,5,7.5,0,5.28-1.08,26.58-1.08,32.4a49.54,49.54,0,0,0,.42,7.74v.24c0,2.1-2.58,2.46-5,2.46-1.68,0-3.9-.18-6.72-.42-1.86-.18-2.7-.24-2.7-1.5a1.19,1.19,0,0,1,1.32-1.14Z"

data SimpleLogoT = SimpleLogoT
instance Themeable SimpleLogoT where
  theme c _ = void $
    is c $ do
      has ".letter" .> do
        fill =: blueHighlight

data RegularLogoT = RegularLogoT
instance Themeable RegularLogoT where
  theme c _ = void $
    is c $ do

      has ".bracket" .> do
        fill =: baseWhite

      has ".letter" .> do
        fill =: baseGreen

      has ".ext" .> do
        fill =: baseWhite

      has ".period" .> do
        fill =: baseWhite

