module Data.TreeSearch where

import Data.Tree

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
{-
testTree =
  Node
    1
    [ Node 2 []
    , Node 3 [Node 4 [], Node 1 [Node 11 []], Node 10 []]
    ]

main :: IO ()
main = do
  showTree testTree
  print ps
  mapM_ (showTree . mt . fst) ps
  where
    ps = search (> 2) testTree
    mt p = Data.TreeSearch.modify (\(Node a ch) -> Node (a - 10) ch) p testTree

-}
