module Components.Email where

import Pure.Elm

import Shared.Types

import Components.Icons ( emailLogo, Mini )

emailLink :: Email -> View
emailLink em =
  A <| Href ("mailto:" <> toTxt em) . Attribute "title" "Email" |>
    [ emailLogo <| Themed @Mini ]
