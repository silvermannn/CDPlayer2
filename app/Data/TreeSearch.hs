module Data.TreeSearch where

import qualified Data.IntMap as I
import qualified Data.Map as M 

import Data.Tree
import Data.Maybe

type TreePath = [Int]

showTree :: Show a => Tree a -> IO ()
showTree t = putStrLn $ drawTree $ fmap show t

search :: (a -> Bool) -> Tree a -> [(TreePath, a, [Tree a])]
search f (Node a chs) =
  m [(j : path, tw, chs') | (j, ch) <- zip [0 ..] chs, (path, tw, chs') <- search f ch]
  where
    m =
      if f a
        then (([], a, chs) :)
        else id

modify :: (Tree a -> Tree a) -> TreePath -> Tree a -> Tree a
modify f [] n = f n
modify f (i:is) n@(Node _ sf) =
  n
    { subForest =
        [ if j == i
          then modify f is ch
          else ch
        | (j, ch) <- zip [0 ..] sf
        ]
    }

lookupNode :: TreePath -> Tree a -> (a, [Tree a])
lookupNode [] (Node a ch) = (a, ch)

insertNode :: a -> Tree a -> Tree a
insertNode ni n@(Node _ sf) = n {subForest = sf ++ [Node ni []]}

data LinearTree a p = LinearTree
  { items :: I.IntMap (a, Int)
  , cache :: M.Map p [Int]
  , counter :: Int
  }
  deriving (Show)

emptyLT :: Ord p => [p] -> LinearTree a p
emptyLT ps = LinearTree I.empty (M.fromList $ map (, [])  ps) 0

singletonLT :: Ord p => p -> a -> LinearTree a p
singletonLT p a = LinearTree (I.singleton 0 (a, -1)) (M.singleton p [0]) 1

cachedItemsLT :: Ord p => p -> LinearTree a p -> [Int]
cachedItemsLT p (LinearTree _ c _) = M.findWithDefault [] p c

getItemLT :: LinearTree a p -> Int -> a
getItemLT (LinearTree m _ _) i = fst $ fromJust $ I.lookup i m 

insertLT :: Ord p => a -> Int -> p -> LinearTree a p -> LinearTree a p
insertLT a j p (LinearTree m c i) = LinearTree (I.insert i (a, j) m) (M.alter (alt i) p c) (i + 1)
  where
    alt j Nothing = Just [j]
    alt j (Just js) = Just (j:js)

toTreeLT :: Show a => LinearTree a p -> [Tree String]
toTreeLT (LinearTree m _ _) = toTreeLT' (-1) 
  where
    children parent = filter ((== parent) . snd . snd) $ I.toList m
    toTreeLT' parent = map makeNode $ children parent  
    makeNode (i, (a, _)) = Node (show a) $ toTreeLT' i

