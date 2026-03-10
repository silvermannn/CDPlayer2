module Data.Syntax.DependencyTree where

data DependencyTree a b = DependencyTree
  { node :: a
  , children :: [(b, DependencyTree a b)]
  } deriving (Show)

emptyTree = DependencyTree () []
