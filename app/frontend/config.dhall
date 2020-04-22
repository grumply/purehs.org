let frontend = ../config.dhall
      { name = "frontend"
      , synopsis = "frontend client" 
      }
let deps = 
        [ "base"
        , "containers"
        , "pure"
        , "pure-css"
        , "pure-elm"
        , "pure-json"
        , "pure-router"
        , "pure-styles"
        , "pure-svg"
        , "pure-theme"
        , "pure-try"
        , "pure-txt"
        , "pure-txt-search"
        , "pure-uri"
        , "pure-websocket"
        , "pure-spinners"
        , "shared"
        , "lens"
        ]
in
  frontend //
    { dependencies = deps
    , library =
        { source-dirs = [ "src" ]
        , other-modules = [] : List Text
        }
    , executables =
        { frontend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = [ "frontend" ] # deps
          } 
        }
    }