*pure-stream* implements a view abstraction for streaming resource acquisition by combining layered stream producers with pull-based stream consumers with controllable stepping/stream forcing.

Here is a demonstration of stream construction; transformation via filtering, mapping, and chunking; and lazy streaming.

<pre data-try>
import Pure hiding (Color,color,alias)
import Pure.Stream as S
import Pure.Intersection

newtype Degree = Degree { fromDegree :: Int }

-- An infinite stream of all whole degrees 0-359
degrees :: Applicative f => Stream f Degree
degrees = unfolds 0 $ \n ->  
  let d = Degree (n `mod` 360)
  in more d (n + 1)

-- filter a stream of degrees by aliasing via modulo
alias :: Applicative f => Int -> Stream f Degree -> Stream f Degree
alias n = S.filter (\d -> fromDegree d `mod` n == 0)

newtype Color = Color { fromColor :: Txt }

-- Use a Degree as a hue and compose with 50% saturation and 50% lightness
color :: Degree -> Color
color d = Color h
  where
    h = hsl(toTxt (fromDegree d),(50%),(50%))

-- fmap over a stream, converting Degree to Color
colors :: Applicative f => Stream f Degree -> Stream f Color
colors = fmap color

newtype Swatch = Swatch { fromSwatch :: View }

swatch :: Color -> Swatch
swatch c = Swatch s
  where
    s = 
      Div <| Themed @Swatch . BackgroundColor (fromColor c) |>
        [ fromTxt (fromColor c) ]

-- fmap over a stream, converting Color to Swatch
swatches :: Applicative f => Stream f Color -> Stream f Swatch
swatches = fmap swatch

main = inject body $ stream $
  frameStepper (200px) def 
    { producer = chunksOf 24 (swatches (colors (alias 5 degrees)))
    , consumer = toList . fmap fromSwatch
    }

instance Theme Swatch where
  theme c = void $ is c .> do
    display =: inline-block
    height  =: 200px 
    width   =: 200px
</pre>

Note that the stream is transformed and chunked after definition and before being produced. 

Each time the stream approaches the bottom, a new chunk of 18 swatches is generated.[^1] In this case there is no IO necessary, but a network request, for example, could be used to generate the elements of the stream.

<pre data-try>
import Pure hiding (get)
import Pure.Data.JSON
import Pure.Stream as Stream
import Pure.XHR (get)
import Control.Monad ((>=>))
import Data.Maybe

newtype Dog = Dog Txt
instance FromJSON Dog where
  parseJSON = withObject "Dog" $ 
    (.: "data") >=> (.: "image_url") >=> (pure . Dog)

dogs :: Stream IO Dog
dogs = fmap fromJust $ Stream.filter isJust $ repeatM $ do
  ed <- get "https://api.giphy.com/v1/gifs/random?api_key=ye5tIg1qDLOYeYqyVnGuB6PTIi2xnvxO&tag=dog&rating=g"
  pure (either (const Nothing) Just ed)

gifs :: Stream IO View
gifs = fmap gif dogs
  where 
    gif (Dog d) = Img <| Themed @Dog . Src d

main = inject body $ stream $ frameStepper (200px) def
  { producer = chunksOf 10 gifs 
  , consumer = toList
  }

instance Theme Dog where
  theme c = void $ is c .> do
    display =: block
    height  =: 25vh
</pre>

Network requests often produce a list of resources. In such cases, the stream can be efficiently modified to consistently generate chunks of a certain size, and stepping the stream will necessarily effectuate enough requests to satisfy the chunk size. 

```haskell
rechunk :: Monad f => Int -> Stream f [a] -> Stream f a
rechunk count = chunksOf count . merge
```

[^1]: [frameStepper](pure-stream/0.8.0.0/Pure.Stream/frameStepper) is necessitated here to induce loading because of the hosting iframe; normally [stepper](pure-stream/0.8.0.0/Pure.Stream/stepper) with a [RootMargin](/packages/pure-intersection/latest/Pure.Intersection/pattern%20RootMargin) would be used in a standard application.