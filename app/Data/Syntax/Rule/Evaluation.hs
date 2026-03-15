module Data.Syntax.Rule.Evaluation where

import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Sentence

penaltyForRule :: Int
penaltyForRule = 1

penaltyForDependencyTree :: Int
penaltyForDependencyTree = 0

penaltyForSentence :: Int
penaltyForSentence = 100

penaltyForResultSize :: Int
penaltyForResultSize = 2

evaluateRulesAlone :: RuleSet -> Int
evaluateRulesAlone (RuleSet rs) = length rs * penaltyForRule

evaluateResult :: DependencyTree -> Result -> Int
evaluateResult target (Result dt s) =
  calcDependancyTreeDifference target dt `div` 100 * penaltyForDependencyTree
    + sentenceSize s * penaltyForSentence

evaluateResults :: DependencyTree -> [Result] -> RuleSet -> Int
evaluateResults target results rs =
  penaltyForResultSize * length results + (minimum $ map evaluateResult' results)
  where
    evaluateResult' result = evaluateRulesAlone rs + evaluateResult target result `div` 10
