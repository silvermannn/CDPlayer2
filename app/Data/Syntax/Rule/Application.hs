module Data.Syntax.Rule.Application where

import Data.TreeSearch

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Predicate
import Data.Syntax.Sentence

data Result =
  Result DependencyTree Sentence
  deriving (Show)

applyRule :: Rule -> Result -> [Result]
applyRule (FindRoot ep) (Result (DependencyTree Nothing) s) =
  [ Result (DependencyTree $ Just $ singletonLT (t, getRootRelation)) (removeUsed t s)
  | t <- filterBy (predicateFilter ep Nothing) s
  ]
applyRule (FindLink ep cp _ _ r) (Result (DependencyTree (Just dt)) s) =
  [ Result (DependencyTree $ Just $ insertLT (t, r) i dt) (removeUsed t s)
  | let (t', i) = lastAddedLT dt
  , predicateFilter ep Nothing $ fst t'
  , t <- filterBy (predicateFilter cp (Just t')) s
  ]
applyRule _ _ = []

applyRules :: RuleSet -> Result -> [Result]
applyRules (RuleSet rs) r = concatMap (`applyRule` r) rs

isFinalResult :: Result -> Bool
isFinalResult (Result _ s) = isEmptySentence s

cyclicApplication :: RuleSet -> Result -> [Result]
cyclicApplication rs start =
  concat $ takeWhile (not . null) $ iterate (concatMap $ applyRules rs) [start]

parseSentence :: RuleSet -> Sentence -> [Result]
parseSentence rs s = cyclicApplication rs (Result emptyDependencyTree s)
