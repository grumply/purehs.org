module Shared where

import Shared.Cache (Cache)
import Shared.Doc (Doc)
import Shared.Package (Package)
import Shared.Page (Page)
import Shared.Post (Post)
import Shared.Tutorial (Tutorial)

import Pure.Data.Txt (Txt)
import Pure.WebSocket (mkRequest,mkMessage,(<:>),none)
import qualified Pure.WebSocket as WS (api)

import Data.Map as Map (Map,toList,fromList,union)
import Data.Set as Set (Set,toList,fromList,union)

host :: String
-- host = "159.65.79.222"
host = "192.168.1.26"

port :: Int
port = 8081

mkRequest "GetPost"     [t|Txt       -> Maybe Post    |]
mkRequest "GetTutorial" [t|Txt       -> Maybe Tutorial|]
mkRequest "GetDoc"      [t|(Txt,Txt) -> Maybe Doc     |]
mkRequest "GetPage"     [t|Txt       -> Maybe Page    |]
mkRequest "GetPackage"  [t|Txt       -> Maybe Package |]

api = WS.api none (getPost <:> getTutorial <:> getDoc <:> getPage <:> getPackage <:> none)

mkMessage "SetCache" [t|Cache|]

clientApi = WS.api (setCache <:> none) none
