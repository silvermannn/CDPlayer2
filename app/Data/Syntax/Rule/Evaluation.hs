module Data.Syntax.Rule.Evaluation where

import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Sentence

penaltyForRule :: Int
penaltyForRule = 5

penaltyForDependencyTree :: Int
penaltyForDependencyTree = 10

penaltyForSentence :: Int
penaltyForSentence = 20

evaluateRulesAlone :: RuleSet -> Int
evaluateRulesAlone (RuleSet rs) = length rs * penaltyForRule

evaluateResult :: DependencyTree -> Result -> Int
evaluateResult target (Result dt s) =
  calcDependancyTreeDifference target dt * penaltyForDependencyTree
    + sentenceSize s * penaltyForSentence

evaluateResults :: DependencyTree -> [Result] -> RuleSet -> Int
evaluateResults target results rs = minimum $ map evaluateResult' results
  where
    evaluateResult' result = evaluateRulesAlone rs + evaluateResult target result
