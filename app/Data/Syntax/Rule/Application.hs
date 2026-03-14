module Data.Syntax.Rule.Application where

import Data.Tree
import Data.TreeSearch

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Sentence
import Data.Syntax.Tag

predicateFilter (Predicate tagId fs) _ (SWord _ _ tagId' fs') = tagId == tagId'

applyRule :: Rule -> Result -> [Result]
applyRule (FindRoot ep) (Result (DependencyTree Nothing) s) =
  [ Result (DependencyTree $ Just $ Node (t, getRootRelation) []) (removeUsed t s)
  | t <- filterBy (predicateFilter ep Nothing) s
  ]
applyRule (FindLink ep cp _ _ r) (Result (DependencyTree (Just dt)) s) =
  [ Result (DependencyTree $ Just $ modify (insertNode (t, r)) path dt) (removeUsed t s)
  | (path, t') <- search (predicateFilter ep Nothing . fst) dt
  , t <- filterBy (predicateFilter cp (Just t')) s
  ]
applyRule _ _ = []

applyRules :: RuleSet -> Result -> [Result]
applyRules (RuleSet rs) r = concatMap (`applyRule` r) rs

isFinalResult :: Result -> Bool
isFinalResult (Result _ (Sentence tws)) = null tws

cyclicApplication :: RuleSet -> Result -> [Result]
cyclicApplication rs start =
  concatMap id --(filter isFinalResult)
    $ takeWhile (not . null)
    $ iterate (concatMap $ applyRules rs) [start]

parseSentence :: RuleSet -> Sentence -> [Result]
parseSentence rs s = cyclicApplication rs (Result emptyDependencyTree s)
