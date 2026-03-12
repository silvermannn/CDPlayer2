module Data.Syntax.Random where

import System.Random

import Data.Syntax.TreeBuildRule

data RuleGenerationParams = RuleGenerationParams
  { maxDistance :: Int
  } deriving (Show)

generateRandomRuleSet ::
     RandomGen g => RuleGenerationParams -> g -> Int -> (RuleSet, g)
generateRandomRuleSet p g n = (RuleSet $ map (int2Rule p) xs, g)
  where
    (xs, g) = uniformList n g

int2Rule :: RuleGenerationParams -> Int -> Rule
int2Rule p = undefined

mutateRuleSet :: RandomGen g => g -> RuleSet -> ([Rule], g)
mutateRuleSet = undefined

crossover2RuleSets :: RandomGen g => g -> RuleSet -> RuleSet -> (Rule, g)
crossover2RuleSets = undefined
