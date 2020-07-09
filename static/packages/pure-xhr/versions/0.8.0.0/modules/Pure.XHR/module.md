
## get

Perform a `Get` request, producing either an [XHRError](Pure.XHR.Utils/data%20XHRError) or a JSON value.

```haskell
get :: FromJSON a => Txt -> IO (Either XHRError a)
```

<div class="hide">
<pre data-try>
import Pure hiding (Value,get)
import Pure.XHR (get)
import Pure.Data.JSON (Value,pretty)

main = do
  posts <- get @Value "https://jsonplaceholder.typicode.com/posts/1"
  print (either (const "Problem with endpoint") pretty posts)
</pre>
</div>

## getRaw

Perform a `Get` request, producing either an [XHRError](Pure.XHR.Utils/data%20XHRError) or the raw [Txt](/packages/pure-txt/0.8.0.0/Pure.Data.Txt/type%20Txt).

```haskell
getRaw :: Txt -> IO (Either XHRError Txt)
```

<div class="hide">
<pre data-try>
import Pure
import Pure.XHR (getRaw)

main = do
  posts <- getRaw "https://jsonplaceholder.typicode.com/posts/1"
  print (either (const "Problem with endpoint") id posts)
</pre>
</div>

## post

Perform a `Post` request, producing either an [XHRError](Pure.XHR.Utils/data%20XHRError) or a JSON value.

```haskell
post :: (ToJSON a,FromJSON b) => Txt -> a -> IO (Either XHRError b)
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.XHR (post)
import Pure.Data.JSON
import GHC.Generics

data Post = Post
  { title  :: Txt
  , userId :: Int
  , body   :: Txt
  } deriving (Generic,ToJSON,FromJSON)

mypost = Post "My Post" 1 "Some post content."

main = do
  post <- post "https://jsonplaceholder.typicode.com/posts" mypost
  print $
    either (const "Problem with endpoint") 
      (pretty @Post) 
      post
</pre>
</div>

## postRaw

Perform a `Post` request, producing either an [XHRError](Pure.XHR.Utils/data%20XHRError) or the raw [Txt](/packages/pure-txt/0.8.0.0/Pure.Data.Txt/type%20Txt) response.

```haskell
postRaw :: ToJSON a => Txt -> a -> IO (Either XHRError Txt)
```

<div class="hide">
<pre data-try>
{-# language DeriveAnyClass #-}
import Pure
import Pure.XHR (postRaw)
import Pure.Data.JSON
import GHC.Generics

data Post = Post
  { title  :: Txt
  , userId :: Int
  , body   :: Txt
  } deriving (Generic,ToJSON)

mypost = Post "My Post" 1 "Some post content."

main = do
  post <- postRaw "https://jsonplaceholder.typicode.com/posts" (encode mypost)
  print $
    either (const "Problem with endpoint") 
      id
      post
</pre>
</div>

## postForm

Submit a form via an XML Http Request, and return either an [XHRError](Pure.XHR.Utils/data%20XHRError) or the response JSON value. `postForm` will handle url encoding.

```haskell
postForm :: (FromJSON b) => Txt -> [(Txt,Txt)] -> IO (Either XHRError b)
```

## postFormRaw

Submit a form via an XML Http Request, and return either an [XHRError](Pure.XHR.Utils/data%20XHRError) or the raw [Txt](/packages/pure-txt/0.8.0.0/Pure.Data.Txt/type%20Txt) response. `postFormRaw` will handle url encoding.

```haskell
postFormRaw :: Txt -> [(Txt,Txt)] -> IO (Either XHRError Txt)
```
