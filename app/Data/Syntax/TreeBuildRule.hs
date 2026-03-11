module Data.Syntax.TreeBuildRule where

import Data.Syntax.DependencyRelation
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
  | FindLink SearchDirection Predicate Predicate DependencyRelation
  deriving (Show)

data Result b =
  Result DependencyTree Sentence
  deriving (Show)

predicate2filter1 (ExactTag _ _) t = True

predicate2filter2 (ExactTag _ _) t1 t2 = True

applyRule :: Rule a -> Result b -> [Result b]
applyRule (FindRoot p) (Result dt s) =
  [ Result (insertNode t getRootRelation dt) (removeUsed t s)
  | t <- filterBy (predicate2filter1 p) s
  ]
applyRule (FindLink dir p1 p2 r) (Result dt s) =
  [ Result (cont (insertNode t r)) (removeUsed t s)
  | (cont, t') <- searchAndModifyNode (predicate2filter1 p1) dt
  , t <- filterBy (predicate2filter2 p2 t') s
  ]

applyRules :: [Rule a] -> Result b -> [Result b]
applyRules rs r = concatMap (`applyRule` r) rs

isFinalResult :: Result b -> Bool
isFinalResult (Result _ (Sentence tws)) = null tws

application :: [Rule a] -> [Result b] -> [Result b]
application rs = concatMap (applyRules rs)

cycle :: [Rule a] -> Result b -> [Result b]
cycle rs start =
  concatMap (filter isFinalResult)
    $ takeWhile (not . null)
    $ iterate (application rs) [start]
