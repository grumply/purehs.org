let test = ../config.dhall
      { name = "test"
      , synopsis = "executable tests"
      }

let deps = (../shared/config.dhall).dependencies 
         # (../frontend/config.dhall).dependencies 
         # (../backend/config.dhall).dependencies
         # [ "pure-test"
           , "pure-bench" 
           , "frontend"
           , "shared"
           , "backend"
           ]

in
  test //
    { dependencies = deps
    , executables = 
        { test = 
          { source-dirs = ["src"] 
          , main = "Main.hs" 
          , dependencies = deps
          }
        }
    }

