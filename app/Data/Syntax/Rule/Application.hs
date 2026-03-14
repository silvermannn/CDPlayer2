module Data.Syntax.Rule.Application where

import qualified Data.Map as M

import Data.Bifunctor
import Data.Tree

import Data.TreeSearch

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Sentence
import Data.Syntax.Tag

type PredicateCache = M.Map Predicate [TreePath]

data Result =
  Result DependencyTree Sentence PredicateCache
  deriving (Show)

initialCache :: RuleSet -> Sentence -> PredicateCache
initialCache (RuleSet rs) s = M.fromList $ map (initial . extractPredicate) rs
  where
    extractPredicate (FindRoot ep) = ep
    extractPredicate (FindLink ep _ _ _ _) = ep
    initial ep = (ep, [])

updateCache :: TaggedWord -> TreePath -> PredicateCache -> PredicateCache
updateCache t path cache = M.mapWithKey (checkPredicate t path) cache
  where
    checkPredicate t path pred paths =
      if predicateFilter pred Nothing t
        then (path : paths)
        else paths

getCached :: Predicate -> PredicateCache -> TreePath
getCached = undefined

predicateFilter (Predicate tagId fs) _ (SWord _ _ tagId' fs') = tagId == tagId'

applyRule :: Rule -> Result -> [Result]
applyRule _ (Result _ (Sentence []) _) = []
applyRule (FindRoot ep) (Result (DependencyTree Nothing) s c) =
  [ Result
    (DependencyTree $ Just $ Node (t, getRootRelation) [])
    (removeUsed t s)
    (updateCache t [] c)
  | t <- filterBy (predicateFilter ep Nothing) s
  ]
applyRule (FindLink ep cp _ _ r) (Result (DependencyTree (Just dt)) s c) =
  [ Result
    (DependencyTree $ Just $ modify (insertNode (t, r)) path dt)
    (removeUsed t s)
    (updateCache t (i : path) c)
  | (path, t', chs) <- search (predicateFilter ep Nothing . fst) dt
  , t <- filterBy (predicateFilter cp (Just t')) s
  , let i = length chs
  ]
applyRule _ _ = []

applyRules :: RuleSet -> Result -> [Result]
applyRules (RuleSet rs) r = concatMap (`applyRule` r) rs

isFinalResult :: Result -> Bool
isFinalResult (Result _ (Sentence tws) _) = null tws

cyclicApplication :: RuleSet -> Result -> [Result]
cyclicApplication rs start =
  concatMap id --(filter isFinalResult)
    $ takeWhile (not . null)
    $ iterate (concatMap $ applyRules rs) [start]

parseSentence :: RuleSet -> Sentence -> [Result]
parseSentence rs s = cyclicApplication rs (Result emptyDependencyTree s (initialCache rs s))
