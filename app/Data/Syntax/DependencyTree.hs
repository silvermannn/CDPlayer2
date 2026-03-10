module Data.Syntax.DependencyTree where

import Data.Tree

data DependencyTree a b = DependencyTree
  { node :: a
  , children :: [(b, DependencyTree a b)]
  } deriving (Show)

singleton ::  a -> DependencyTree a b
singleton a = DependencyTree a []

toTree :: b -> DependencyTree a b -> Tree (b, a)
toTree root dt = go (root, dt)
  where
    go (r, DependencyTree n chs) = Node (r, n) $ map go chs

showDependencyTree :: (Show a, Show b) => b -> DependencyTree a b -> IO ()
showDependencyTree root dt = putStrLn $ drawTree $ fmap show $ toTree root dt
