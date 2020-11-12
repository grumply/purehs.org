let frontend = ../config.dhall
      { name = "frontend"
      , synopsis = "frontend client" 
      }
let deps = 
        [ "base"
        , "async"
        , "containers"
        , "pure"
        , "pure-css"
        , "pure-elm"
        , "pure-json"
        , "pure-lifted"
        , "pure-maybe"
        , "pure-random-pcg"
        , "pure-render"
        , "pure-router"
        , "pure-styles"
        , "pure-svg"
        , "pure-theme"
        , "pure-time"
        , "pure-try"
        , "pure-txt"
        , "pure-txt-interpolate"
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
    , executables =
        { frontend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = deps
          } 
        }
    }