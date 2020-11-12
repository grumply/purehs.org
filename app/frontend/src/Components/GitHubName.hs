module Components.GitHubName where

import Pure.Elm

import Shared.Types

import Components.Icons ( gitHubLogo_alt, Mini )

gitHubNameLink :: GitHubName -> View
gitHubNameLink gh =
  A <| Href (toTxt gh) . Attribute "target" "_blank" . Rel "noopener" . Attribute "title" "GitHub" |>
    [ gitHubLogo_alt <| Themed @Mini ]

