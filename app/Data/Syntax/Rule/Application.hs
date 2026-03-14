module Data.Syntax.Rule.Application where

import qualified Data.Map as M

import Data.Bifunctor
import Data.Tree

import Data.TreeSearch

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule.Predicate
import Data.Syntax.Rule
import Data.Syntax.Sentence
import Data.Syntax.Tag

data Result =
  Result DependencyTree Sentence 
  deriving (Show)

predicateFilter (Predicate tagId fs) _ (SWord _ _ tagId' fs') = tagId == tagId'

applyRule :: Rule -> Result -> [Result]
applyRule _ (Result _ (Sentence [])) = []
applyRule (FindRoot ep) (Result (DependencyTree Nothing) s) =
  [ Result
    (DependencyTree $ Just $ singletonLT ep (t, getRootRelation))
    (removeUsed t s)
  | t <- filterBy (predicateFilter ep Nothing) s
  ]
applyRule (FindLink ep cp _ _ r) (Result (DependencyTree (Just dt)) s) =
  [ Result
    (DependencyTree $ Just $ insertLT (t, r) i ep dt)
    (removeUsed t s)
  | let is = cachedItemsLT ep dt
  ,  i <- is
  ,  let (t', _) =  getItemLT dt i
    --(path, t', chs) <- search (predicateFilter ep Nothing . fst) dt
  , t <- filterBy (predicateFilter cp (Just t')) s
  --, let i = length chs
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
