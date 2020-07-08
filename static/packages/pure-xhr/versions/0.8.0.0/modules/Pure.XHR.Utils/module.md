## data XHRError

The error type for all XHR endpoints. In GHC, the `GHCNotSupported` error will be returned from all endpoints.

```haskell
data XHRError 
  = StatusError Txt Int
  | ParseError Txt String
  | InvalidURLError Txt Txt
  | OtherError Txt SomeException
  | GHCNotSupported
```

## xhrErrorURL 

Extractor for the URL of the endpoint that errored.

```haskell
xhrErrorURL :: XHRError -> Txt
```