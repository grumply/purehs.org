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

host = "159.65.79.222"
port = 8081

asMap :: Ord k => [(k,v)] -> (Map k v -> Map k v) -> [(k,v)]
asMap kvs f = Map.toList $ f $ Map.fromList kvs

asSet :: Ord k => [k] -> (Set k -> Set k) -> [k]
asSet ks f = Set.toList $ f $ Set.fromList ks

mkRequest "GetPost"     [t|Txt       -> Maybe Post    |]
mkRequest "GetTutorial" [t|Txt       -> Maybe Tutorial|]
mkRequest "GetDoc"      [t|(Txt,Txt) -> Maybe Doc     |]
mkRequest "GetPage"     [t|Txt       -> Maybe Page    |]
mkRequest "GetPackage"  [t|Txt       -> Maybe Package |]

api = WS.api none (getPost <:> getTutorial <:> getDoc <:> getPage <:> none)

mkMessage "SetCache" [t|Cache|]

clientApi = WS.api (setCache <:> none) none