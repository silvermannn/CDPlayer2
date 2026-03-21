module Data.FastTree where

import Debug.Trace

import qualified Data.IntMap.Strict as I

import Data.List
import Data.Maybe
import Data.Tree
import Data.Tuple

data FastTree a = FastTree
  { items :: I.IntMap (a, Int)
  , lastAdded :: [(a, Int)]
  , fringe :: [(a, Int)]
  , counter :: Int
  } deriving (Show)

singletonLT :: a -> FastTree a
singletonLT a = FastTree (I.singleton 0 (a, -1)) [(a, 0)] [] 1

fringeLT :: FastTree a -> [(a, Int)]
fringeLT (FastTree is _ _ _) = map (\(i, (a, _)) -> (a, i)) $ I.toList is

updateFringeLT :: FastTree a -> FastTree a
updateFringeLT (FastTree m l f i) = (FastTree m [] l i)

getItemLT :: FastTree a -> Int -> a
getItemLT (FastTree m _ _ _) i = fst $ fromJust $ I.lookup i m

insertLT :: a -> Int -> FastTree a -> FastTree a
insertLT a j (FastTree m l f i) = FastTree (I.insert i (a, j) m) ((a, i) : l) f (i + 1)

children :: I.IntMap (a, Int) -> Int -> [(Int, (a, Int))]
children m parent = filter ((== parent) . snd . snd) $ I.toList m

toTreeLT :: (Show a, Eq a) => FastTree a -> [Tree String]
toTreeLT (FastTree m l f _) = toTreeLT' (-1)
  where
    toTreeLT' parent = map makeNode $ children m parent
    makeNode (i, (a, _)) = Node (showNode a) $ toTreeLT' i
    showNode a =
      show a
        ++ if a `elem` map fst l
             then " *"
             else ""
                    ++ if a `elem` map fst f
                         then " <<--"
                         else ""

differenceLT :: (Eq a, Ord a) => FastTree a -> FastTree a -> Int
differenceLT (FastTree is1 _ _ _) (FastTree is2 _ _ _) = differenceLT' (-1) (-1)
  where
    differenceLT' parent1 parent2 = dl * dl + (sum $ zipWith compare chs1 chs2)
      where
        chs1 = sort $ children is1 parent1
        chs2 = sort $ children is2 parent2
        dl = abs $ length chs1 - length chs2
        compare (i1, (a1, _)) (i2, (a2, _)) =
          differenceLT' i1 i2
            + if a1 == a2
                then 0
                else 1

sizeLT :: FastTree a -> Int
sizeLT (FastTree _ _ _ c) = c
