module Data.Syntax.TreeBuildRule where

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Tag

data SearchDirection
  = SearchLeft Int
  | SearchRight Int
  deriving (Show)

data ExactPredicate =
  ExactPredicate Int [(Int, Int)]
  deriving (Show)

data CorrespondentPredicate =
  CorrespondentPredicate SearchDirection Int [(Int, Maybe Int)]
  deriving (Show)

data Rule
  = FindRoot ExactPredicate
  | FindLink ExactPredicate CorrespondentPredicate DependencyRelation
  deriving (Show)

data Result =
  Result DependencyTree Sentence
  deriving (Show)

exactFilter (ExactPredicate tagId _) (SWord _ _ tagId' _) = tagId == tagId'

correspondentFilter (CorrespondentPredicate _ tagId _) (SWord _ _ _ _) (SWord _ _ tagId2 _) =
  tagId == tagId2

applyRule :: Rule -> Result -> [Result]
applyRule (FindRoot ep) (Result (DependencyTree Nothing) s) =
  [ Result
    (DependencyTree $ Just $ DependencyTreeNode t getRootRelation [])
    (removeUsed t s)
  | t <- filterBy (exactFilter ep) s
  ]
applyRule (FindLink ep cp r) (Result (DependencyTree (Just dt)) s) =
  [ Result (DependencyTree $ Just $ cont (insertNode t r)) (removeUsed t s)
  | (cont, t') <- searchAndModifyNode (exactFilter ep) dt
  , t <- filterBy (correspondentFilter cp t') s
  ]
applyRule _ _ = []

applyRules :: [Rule] -> Result -> [Result]
applyRules rs r = concatMap (`applyRule` r) rs

isFinalResult :: Result -> Bool
isFinalResult (Result _ (Sentence tws)) = null tws

application :: [Rule] -> [Result] -> [Result]
application rs = concatMap (applyRules rs)

cyclicApplication :: [Rule] -> Result -> [Result]
cyclicApplication rs start =
    concatMap id --(filter isFinalResult)
    $ takeWhile (not . null)
    $ iterate (application rs) [start]
