Make HTTP requests.

<pre data-try>
import Pure hiding (Right,get)
import Pure.Data.JSON hiding (Null)
import Pure.XHR (get)
import Control.Monad ((>=>))

newtype Dog = Dog Txt
instance FromJSON Dog where
  parseJSON = withObject "Dog" $ 
    (.: "data") >=> (.: "image_url") >=> (pure . Dog)

main = do
  dog <- get "https://api.giphy.com/v1/gifs/random?api_key=ye5tIg1qDLOYeYqyVnGuB6PTIi2xnvxO&tag=dog&rating=g"
  inject body $
    case dog of
      Right (Dog d) -> Img <| Src d
      _             -> "🐶 not found."
</pre>