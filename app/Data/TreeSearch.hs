module Data.TreeSearch where

import qualified Data.IntMap.Strict as I

import Data.Maybe
import Data.Tree

data FastTree a = FastTree
  { items :: I.IntMap (a, Int)
  , lastAdded :: (a, Int)
  , counter :: Int
  } deriving (Show)

singletonLT :: a -> FastTree a
singletonLT a = FastTree (I.singleton 0 (a, -1)) (a, 0) 1

lastAddedLT :: FastTree a -> (a, Int)
lastAddedLT (FastTree _ l _) = l

getItemLT :: FastTree a -> Int -> a
getItemLT (FastTree m _ _) i = fst $ fromJust $ I.lookup i m

insertLT :: a -> Int -> FastTree a -> FastTree a
insertLT a j (FastTree m _ i) = FastTree (I.insert i (a, j) m) (a, i) (i + 1)

toTreeLT :: (Show a, Eq a) => FastTree a -> [Tree String]
toTreeLT (FastTree m (l, _) _) = toTreeLT' (-1)
  where
    children parent = filter ((== parent) . snd . snd) $ I.toList m
    toTreeLT' parent = map makeNode $ children parent
    makeNode (i, (a, _)) = Node (showNode a) $ toTreeLT' i
    showNode a =
      if a == l
        then show a ++ " <<--"
        else show a

differenceLT :: FastTree a -> FastTree a -> Int
differenceLT (FastTree is1 _ c1) (FastTree is2 _ c2) = I.size (I.intersection is1 is2)

sizeLT :: FastTree a -> Int
sizeLT (FastTree _ _ c) = c
