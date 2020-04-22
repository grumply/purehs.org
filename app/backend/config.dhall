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
      , "pure-websocket"
      , "shared"
      , "wai"
      , "wai-app-static"
      , "wai-extra"
      , "warp"
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
