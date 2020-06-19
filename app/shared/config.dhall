let shared = ../config.dhall
      { name = "shared"
      , synopsis = "shared types and apis" 
      }
let deps = [ "base"
           , "bytestring"
           , "containers"
           , "hashable"
           , "pure"
           , "pure-core"
           , "pure-elm"
           , "pure-json"
           , "pure-render"
           , "pure-time"
           , "pure-try"
           , "pure-txt"
           , "pure-txt-interpolate"
           , "pure-txt-search"
           , "pure-websocket"
           , "time"
           , "Glob"
           , "yaml"
           ]
in
  shared //
    { dependencies = deps
    , library = 
        { source-dirs = ["src " ]
        , other-modules = [] : List Text
        }
    }