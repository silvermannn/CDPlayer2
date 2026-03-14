module Data.Syntax.DependencyTree where

import Data.Bifunctor
import Data.Maybe
import Data.Tree

import Data.Syntax.DependencyRelation
import Data.Syntax.Tag
import Data.TreeSearch
import Data.Syntax.Rule.Predicate

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

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (a:as) = scanl (\(xs, x, y:ys) _ -> (x : xs, y, ys)) ([], a, as) as

searchTree :: Predicate -> DependencyTree -> [Int]
searchTree p (DependencyTree mn) = maybe [] (cachedItemsLT p) mn

insertTree ::
     DependencyTree -> Predicate -> NodeInfo -> Int -> DependencyTree
insertTree dt@(DependencyTree mn) p n i = maybe dt (DependencyTree . Just . insertLT n i p) mn

calcDependancyTreeDifference :: DependencyTree -> DependencyTree -> Int
calcDependancyTreeDifference dt1 dt2 = 0
