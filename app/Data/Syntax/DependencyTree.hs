module Data.Syntax.DependencyTree where

import Data.Maybe
import Data.Tree

import Data.Syntax.DependencyRelation
import Data.Syntax.Rule.Predicate
import Data.Syntax.Tag
import Data.TreeSearch

newtype DependencyTree =
  DependencyTree (Maybe DependencyTreeNode)
  deriving (Show)

type NodeInfo = (TaggedWord, DependencyRelation)

type DependencyTreeNode = LinearTree NodeInfo Predicate

emptyDependencyTree :: DependencyTree
emptyDependencyTree = DependencyTree Nothing

dependencyTreeToTree :: DependencyTree -> Tree String
dependencyTreeToTree (DependencyTree mch) = Node "<root>" $ maybe [] toTreeLT mch

showDependencyTree :: DependencyTree -> IO ()
showDependencyTree dt = putStrLn $ drawTree $ dependencyTreeToTree dt

calcDependancyTreeDifference :: DependencyTree -> DependencyTree -> Int
calcDependancyTreeDifference dt1 dt2 = 0
