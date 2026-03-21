module Data.Syntax.DependencyTree where

import Data.Tree

import Data.FastTree
import Data.Syntax.DependencyRelation
import Data.Syntax.Tag

newtype DependencyTree =
  DependencyTree (Maybe DependencyTreeNode)
  deriving (Show)

type NodeInfo = (TaggedWord, DependencyRelation)

type DependencyTreeNode = FastTree NodeInfo

emptyDependencyTree :: DependencyTree
emptyDependencyTree = DependencyTree Nothing

dependencyTreeToTree :: DependencyTree -> Tree String
dependencyTreeToTree (DependencyTree mch) = Node "<root>" $ maybe [] toTreeLT mch

showDependencyTree :: DependencyTree -> IO ()
showDependencyTree dt = putStrLn $ drawTree $ dependencyTreeToTree dt

updateFringe :: DependencyTree -> DependencyTree
updateFringe (DependencyTree (Just dt)) = DependencyTree $ Just $ updateFringeLT dt
updateFringe dt = dt

calcDependancyTreeDifference :: DependencyTree -> DependencyTree -> Int
calcDependancyTreeDifference (DependencyTree Nothing) (DependencyTree Nothing) = 0
calcDependancyTreeDifference (DependencyTree Nothing) (DependencyTree (Just dt)) = 100000
calcDependancyTreeDifference (DependencyTree (Just dt)) (DependencyTree Nothing) = 100000
calcDependancyTreeDifference (DependencyTree (Just dt1)) (DependencyTree (Just dt2)) =
  differenceLT dt1 dt2
