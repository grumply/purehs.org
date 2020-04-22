let shared = ../config.dhall
      { name = "shared"
      , synopsis = "shared types and apis" 
      }
in
  shared //
    { dependencies =
        [ "base"
        , "bytestring"
        , "containers"
        , "hashable"
        , "pure"
        , "pure-core"
        , "pure-elm"
        , "pure-json"
        , "pure-render"
        , "pure-try"
        , "pure-txt"
        , "pure-websocket"
        ]
    , library = 
        { source-dirs = ["src"]
        , other-modules = [] : List Text
        }
    }