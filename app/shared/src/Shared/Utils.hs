module Shared.Utils where

import Data.Map as Map (Map,toList,fromList,union)
import Data.Set as Set (Set,toList,fromList,union)

import Pure.Data.Txt as Txt

import Data.Bits
import Data.Char
import Data.Word

asMap :: Ord k => [(k,v)] -> (Map k v -> Map k v) -> [(k,v)]
asMap kvs f = Map.toList $ f $ Map.fromList kvs

asSet :: Ord k => [k] -> (Set k -> Set k) -> [k]
asSet ks f = Set.toList $ f $ Set.fromList ks

unionAsSet :: Ord a => [a] -> [a] -> [a]
unionAsSet l r = Set.toList $ Set.union (Set.fromList l) (Set.fromList r)

unionAsMap :: Ord a => [(a,b)] -> [(a,b)] -> [(a,b)]
unionAsMap l r = Map.toList $ Map.union (Map.fromList l) (Map.fromList r)

breakMany :: (a -> Bool) -> [a] -> ([a],[[a]])
breakMany f = Prelude.foldr (\x (xs,xxs) -> if f x then ([],(x:xs):xxs) else (x:xs,xxs)) ([],[])

-- Data.Hashable doesn't work across GHC and GHCJS, so we need a 
-- custom hash with a fixed-width integral type.
{-# INLINE fnv64 #-}
fnv64 :: Txt -> Word64
fnv64 = Txt.foldl' hash 0xcbf29ce484222325
  where
    {-# INLINE hash #-}
    hash :: Word64 -> Char -> Word64
    hash i c = 
      let i' = i `xor` fromIntegral (ord c) 
      in i' * 0x100000001b3
