module Data.Syntax.TreeBuildRule where

import Data.Syntax.DependencyTree
import Data.Syntax.Sentence

data SearchDirection
  = SearchLeft Int
  | SearchRight Int
  deriving (Show)

data Predicate
  = ExactTag Int [(Int, Int)]
  | Correspondence Int [Int]
  deriving (Show)

data Rule a
  = FindRoot Predicate
  | FindLink SearchDirection Predicate Predicate
  deriving (Show)

data Result b =
  Result (DependencyTree TaggedWord b) Sentence

applyRule :: Rule a -> Result b -> [Result b]
applyRule (FindRoot _) r = undefined
applyRule (FindLink _ _ _) r = undefined

applyRules :: [Rule a] -> Result b -> [Result b]
applyRules rs r = concatMap (flip applyRule r) rs

isFinalResult :: Result b -> Bool
isFinalResult (Result _ (Sentence tws)) = null tws

application :: [Rule a] -> [Result b] -> [Result b]
application rs ress = concatMap (applyRules rs) ress

cycle rs start = filter isFinalResult $ concat $ takeWhile (not . null) $ iterate (application rs) [start]

