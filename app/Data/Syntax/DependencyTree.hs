module Data.Syntax.DependencyTree where

import Data.Maybe
import Data.Tree

import Data.Syntax.DependencyRelation
import Data.Syntax.Tag

newtype DependencyTree =
  DependencyTree (Maybe DependencyTreeNode)
  deriving (Show)

data DependencyTreeNode = DependencyTreeNode
  { node :: TaggedWord
  , relation :: DependencyRelation
  , children :: [DependencyTreeNode]
  } deriving (Show)

emptyDependencyTree :: DependencyTree
emptyDependencyTree = DependencyTree Nothing

nodeToTree :: DependencyTreeNode -> Tree String
nodeToTree (DependencyTreeNode n r chs) = Node (show (r, n)) $ map nodeToTree chs

showDependencyTreeNode :: DependencyTreeNode -> IO ()
showDependencyTreeNode dt = putStrLn $ drawTree $ nodeToTree dt

dependencyTreeToTree :: DependencyTree -> Tree String
dependencyTreeToTree (DependencyTree mch) = Node "<root>" $ maybe [] (pure . nodeToTree) mch

showDependencyTree :: DependencyTree -> IO ()
showDependencyTree dt = putStrLn $ drawTree $ dependencyTreeToTree dt

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (a:as) = scanl (\(xs, x, y:ys) _ -> (x : xs, y, ys)) ([], a, as) as

searchAndModifyNode ::
     (TaggedWord -> Bool)
  -> DependencyTreeNode
  -> [((DependencyTreeNode -> DependencyTreeNode) -> DependencyTreeNode, TaggedWord)]
searchAndModifyNode p dt@(DependencyTreeNode n r chs) =
  [(\m -> m dt, node dt) | p n]
    ++ [ (\m -> DependencyTreeNode n r (pchs ++ (cont m : achs)), t')
       | (pchs, ch, achs) <- splits chs
       , (cont, t') <- searchAndModifyNode p ch
       ]

searchAndModifyTree ::
     (TaggedWord -> Bool)
  -> DependencyTree
  -> [((DependencyTreeNode -> DependencyTreeNode) -> DependencyTreeNode, TaggedWord)]
searchAndModifyTree p (DependencyTree mch) = maybe [] (searchAndModifyNode p) mch

insertNode :: TaggedWord -> DependencyRelation -> DependencyTreeNode -> DependencyTreeNode
insertNode t r dt = dt {children = DependencyTreeNode t r [] : children dt}

calcDependancyTreeDifference :: DependencyTree -> DependencyTree -> Int
calcDependancyTreeDifference dt1 dt2 = 0
