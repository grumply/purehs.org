module Components.Twitter where

import Pure.Elm

import Shared.Types

import Components.Icons ( twitterLogo, Mini )

twitterHandleLink :: TwitterHandle -> View
twitterHandleLink th =
  A <| Href (toTxt th) . Attribute "target" "_blank" . Rel "noopener" . Attribute "title" "Twitter" |>
    [ twitterLogo <| Themed @Mini ]