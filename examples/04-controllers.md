----------------
title: Controllers
highlights: []
----------------

Pure Controllers are singleton components that endow views with an effectful context. The `context` that is tracked by all views refers to a parent Controller. That context is dynamically generated on initialization by the `build` field of a Controller.

# Code

```haskell
type Name = Txt
type Password = Txt
data Login ctx = Login
  { loginUsername :: Name
  , loginPassword :: Password
  }

_Login = Controller {..}
  where
    key = "Login"
    build = return
    prime = def
    model = Login def def
    view (Login un pw) =
      Form [ onSubmit login ]
        [ Label [] "Username:"
        , Input [ Type "text", Value un, onInput username ] []
        , Label [] "Password:"
        , Input [ Type "password", Value pw, onInput password ] []
        , Input [ Type "submit", Value "Submit" ] []
        ]

    username un = modifyModel $ \Login {..} -> Login { loginUsername = un, .. }
    password pw = modifyModel $ \Login {..} -> Login { loginPassword = pw, .. }

    login = do
      Login {..} <- getModel
      remote tryLogin (loginUsername,loginPassword) handleLoginResult
```
