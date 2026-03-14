module Data.Syntax.Rule.Evaluation where

import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Sentence

penaltyForRule :: Int
penaltyForRule = 1

penaltyForDependencyTree :: Int
penaltyForDependencyTree = 10

penaltyForSentence :: Int
penaltyForSentence = 100

evaluateRulesAlone :: RuleSet -> Int
evaluateRulesAlone (RuleSet rs) = length rs * penaltyForRule

evaluateResult :: DependencyTree -> Result -> Int
evaluateResult target (Result dt (Sentence s) _) =
  calcDependancyTreeDifference target dt * penaltyForDependencyTree + length s * penaltyForSentence

evaluateResults :: DependencyTree -> [Result] -> RuleSet -> Int
evaluateResults target results rs = minimum $ map (evaluateResult' rs target) results
  where
    evaluateResult' rs target result = evaluateRulesAlone rs + evaluateResult target result
