module Data.Syntax.Rule.Random where

import System.Random

import Data.List

import Data.Syntax.Rule

data RuleGenerationParams = RuleGenerationParams
  { maxDistance :: Int
  , tagsSize :: Int
  , maxFeaturePairs :: Int
  , featureNamesSize :: Int
  , featureValuesSize :: Int
  , dependencyRelationsSize :: Int
  } deriving (Show)

generateRandomRuleSet ::
     RandomGen g => RuleGenerationParams -> g -> Int -> (RuleSet, g)
generateRandomRuleSet p g n = (RuleSet $ map (int2Rule p) xs, g')
  where
    (xs, g') = uniformList n g

int2Rule :: RuleGenerationParams -> Int -> Rule
int2Rule p n =
  if rv == 0
    then FindRoot (int2ExactPredicate p ep)
    else FindLink (int2ExactPredicate p ep) (int2CorrespondentPredicate p cp) dr
  where
    (_, [rv, ep, cp, dr]) = generateMods n [2, m, m, dependencyRelationsSize p]
    m = maxPredicateVariant p

maxPredicateVariant :: RuleGenerationParams -> Int
maxPredicateVariant p =
  (tagsSize p)
    * (((featureNamesSize p) * (featureNamesSize p)) ^ (maxFeaturePairs p))
    * (maxDistance p)

int2ExactPredicate :: RuleGenerationParams -> Int -> ExactPredicate
int2ExactPredicate p n = ExactPredicate t ps
  where
    (n1, t) = n `divMod` (dependencyRelationsSize p)
    ps = int2Featurepairs p n1

int2CorrespondentPredicate ::
     RuleGenerationParams -> Int -> CorrespondentPredicate
int2CorrespondentPredicate p n =
  CorrespondentPredicate (sd sdd) t (map (fmap Just) ps)
  where
    (n1, [t, sdi, sdd]) =
      generateMods n [dependencyRelationsSize p, 2, maxDistance p]
    sd =
      if sdi == 0
        then SearchLeft
        else SearchRight
    ps = int2Featurepairs p n1

int2Featurepairs :: RuleGenerationParams -> Int -> [(Int, Int)]
int2Featurepairs p n = zip (nub fns) fvs
  where
    (n1, size) = n `divMod` (maxFeaturePairs p)
    (n2, fns) = generateMods n1 $ replicate size (featureNamesSize p)
    (_, fvs) = generateMods n2 $ replicate size (featureValuesSize p)

generateMods :: Int -> [Int] -> (Int, [Int])
generateMods n xs =
  foldr (\x (n', acc) -> (n' `div` x, (n' `mod` x) : acc)) (n, []) xs

mutateRuleSet :: RandomGen g => g -> RuleSet -> ([Rule], g)
mutateRuleSet = undefined

crossover2RuleSets :: RandomGen g => g -> RuleSet -> RuleSet -> (Rule, g)
crossover2RuleSets = undefined
