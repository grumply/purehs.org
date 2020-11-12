module Components.Company where

import Pure.Elm

import Shared.Types

import Components.Icons ( companyLogo, Mini )

companyLink :: Company -> View
companyLink c =
  A <| Href (toTxt c) . Attribute "target" "_blank" . Rel "noopener" . Attribute "title" "Company" |>
    [ companyLogo <| Themed @Mini ]