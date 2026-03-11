module Data.Syntax.DependencyTree where

import Data.Tree

import Data.Syntax.Tag 
import Data.Syntax.DependencyRelation

data DependencyTree = DependencyTree
  { node :: TaggedWord  , relation :: DependencyRelation
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
  -> (DependencyTree -> DependencyTree)
  -> DependencyTree
  -> [DependencyTree]
searchAndModifyNode p m dt@(DependencyTree n r chs) =
  [m dt | p n]
    ++ [ DependencyTree n r (pchs ++ (ch' : achs))
       | (pchs, ch, achs) <- splits chs
       , ch' <- searchAndModifyNode p m ch
       ]
