module Data.Syntax.DependencyTree where

import Data.Tree

import Data.Syntax.DependencyRelation
import Data.Syntax.Tag

data DependencyTree = DependencyTree
  { node :: TaggedWord
  , relation :: DependencyRelation
  , children :: [DependencyTree]
  } deriving (Show)

singleton :: TaggedWord -> DependencyRelation -> DependencyTree
singleton a r = DependencyTree a r []

toTree :: DependencyTree -> Tree (DependencyRelation, TaggedWord)
toTree (DependencyTree n r chs) = Node (r, n) $ map toTree chs

showDependencyTree :: DependencyTree -> IO ()
showDependencyTree dt = putStrLn $ drawTree $ fmap show $ toTree dt

splits :: [a] -> [([a], a, [a])]
splits [] = []
splits (a:as) = scanl (\(xs, x, y:ys) _ -> (x : xs, y, ys)) ([], a, as) as

searchAndModifyNode ::
     (TaggedWord -> Bool)
  -> DependencyTree
  -> [((DependencyTree -> DependencyTree) -> DependencyTree, TaggedWord)]
searchAndModifyNode p dt@(DependencyTree n r chs) =
  [(\m -> m dt, node dt) | p n]
    ++ [ (\m -> DependencyTree n r (pchs ++ (cont m : achs)), t')
       | (pchs, ch, achs) <- splits chs
       , (cont, t') <- searchAndModifyNode p ch
       ]

insertNode ::
     TaggedWord -> DependencyRelation -> DependencyTree -> DependencyTree
insertNode t r dt = dt {children = DependencyTree t r [] : children dt}
