## Servers and Services

[pure-server](/doc/pure-server/0.7.0.0) eases implementation of Pure servers and services by using the same architecture as Pure frontend applications: they are hierarchical - nestable, and they can use the same components as frontend applications.

```haskell
server = ComponentIO $ \self -> def
  { construct = return someServerState
  , render = \_ st -> 
    Server host port (\ws -> connection (ws,st))
  }

connection = ComponentIO $ \self -> def
  { construct = return someInitialConnectionState
  , executing = someConnectHandler
  , unmount   = someDisconnectHandler
  }
```

Combined with [pure-websocket](/doc/pure-websocket/0.7.0.0), this is a powerful approach to dynamic, full-stack applications.
