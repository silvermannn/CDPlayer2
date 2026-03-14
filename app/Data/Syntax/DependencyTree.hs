module Data.Syntax.DependencyTree where

import Data.Bifunctor
import Data.Maybe
import Data.Tree

import Data.Syntax.DependencyRelation
import Data.Syntax.Tag
import Data.TreeSearch

newtype DependencyTree =
  DependencyTree (Maybe DependencyTreeNode)
  deriving (Show)

type NodeInfo = (TaggedWord, DependencyRelation)

type DependencyTreeNode = Tree NodeInfo

emptyDependencyTree :: DependencyTree
emptyDependencyTree = DependencyTree Nothing

dependencyTreeToTree :: DependencyTree -> Tree String
dependencyTreeToTree (DependencyTree mch) = Node "<root>" $ maybe [] (pure . fmap show) mch

showDependencyTree :: DependencyTree -> IO ()
showDependencyTree dt = putStrLn $ drawTree $ dependencyTreeToTree dt

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (a:as) = scanl (\(xs, x, y:ys) _ -> (x : xs, y, ys)) ([], a, as) as

searchTree :: (NodeInfo -> Bool) -> DependencyTree -> [(TreePath, NodeInfo, [DependencyTreeNode])]
searchTree p (DependencyTree mn) = maybe [] (search p) mn

modifyTree ::
     DependencyTree -> (DependencyTreeNode -> DependencyTreeNode) -> TreePath -> DependencyTree
modifyTree dt@(DependencyTree mn) f path = maybe dt (DependencyTree . Just . modify f path) mn

calcDependancyTreeDifference :: DependencyTree -> DependencyTree -> Int
calcDependancyTreeDifference dt1 dt2 = 0
