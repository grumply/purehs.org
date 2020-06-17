let backend = ../config.dhall
      { name = "backend"
      , synopsis = "backend server"
      }
let deps = 
      [ "base"
      , "bytestring"
      , "containers"
      , "directory"
      , "filepath"
      , "http-types"
      , "pandoc"
      , "pure"
      , "pure-cached"
      , "pure-core"
      , "pure-default"
      , "pure-elm"
      , "pure-json"
      , "pure-render"
      , "pure-server"
      , "pure-tagsoup"
      , "pure-time"
      , "pure-try"
      , "pure-txt"
      , "pure-txt-interpolate"
      , "pure-websocket"
      , "text"
      , "wai"
      , "wai-app-static"
      , "wai-extra"
      , "warp"
      , "yaml"
      , "Glob"
      , "shared"
      ]
in
  backend //
    { dependencies = deps
    , library =
        { source-dirs = [ "src" ]
        , other-modules = [] : List Text
        }
    , executables =
        { backend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = [ "backend" ] # deps
          } 
        }
    }
