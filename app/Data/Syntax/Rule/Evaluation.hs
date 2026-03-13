module Data.Syntax.Rule.Evaluation where

import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Rule

penaltyForRule :: Int
penaltyForRule = 1

penaltyForDependencyTree :: Int
penaltyForDependencyTree = 10

penaltyForSentence :: Int
penaltyForSentence = 100

evaluateRulesAlone :: RuleSet -> Int
evaluateRulesAlone (RuleSet rs) = length rs * penaltyForRule

evaluateResult :: DependencyTree -> Result-> Int
evaluateResult target (Result dt (Sentence s)) = calcDependancyTreeDifference target dt * penaltyForDependencyTree + length s * penaltyForSentence

evaluateApplication :: RuleSet -> DependencyTree -> Result-> Int
evaluateApplication rs target result = evaluateRulesAlone rs + evaluateResult target result

