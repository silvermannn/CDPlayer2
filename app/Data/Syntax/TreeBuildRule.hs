module Data.Syntax.TreeBuildRule where

import Data.Syntax.DependencyTree
import Data.Syntax.Sentence

data SearchDirection
  = SearchLeft Int
  | SearchRight Int
  deriving (Show)

data Rule a
  = FindRoot a
  | FindLink SearchDirection a a
  deriving (Show)

applyRule ::
     Rule a
  -> DependencyTree b TaggedWord
  -> Sentence
  -> [DependencyTree b TaggedWord]
applyRule (FindRoot _) dt s = undefined
applyRule (FindLink _ _ _) dt s = undefined
