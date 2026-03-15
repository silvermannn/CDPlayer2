module Data.Syntax.Rule.Random where

import System.Random

import Control.Monad

import GHC.Generics

import Data.List

import Data.Common
import Data.Random.Stateful
import Data.Syntax.Rule
import Data.Syntax.Rule.Predicate

data RuleGenerationParams = RuleGenerationParams
  { maxDistance :: Int
  , maxTagIndex :: Int
  , maxFeaturePairs :: Int
  , maxFeatureNameIndex :: Int
  , maxFeatureValuesIndex :: Int
  , maxDependencyRelationsIndex :: Int
  } deriving (Show)

data MutationRuleSet
  = DeleteRule
  | InsertRule
  | MutateRule
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)

data MutationPredicate
  = MutatePredicateTag
  | DeletePredicateFeaturePair
  | MutatePredicateFeaturePair
  | AddPredicateFeaturePair
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)

data MutationiRootRule
  = ConvertRootToLink
  | MutateRootPredicate
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)

data MutationLinkRule
  = ConvertLinkToRoot
  | MutateLinkPredicate
  | MutateCorrespondentPredicate
  | MutateSearchDirection
  | MutateSearchDistance
  | MutateDependencyRelation
  deriving (Show, Enum, Bounded, Generic, Finite, Uniform)

generateRuleSet :: RuleGenerationParams -> Int -> StatefulRandom RuleSet
generateRuleSet p size = do
  rs <- replicateM size $ generateRule p
  return $ RuleSet $ nub $ sort rs

generateRule :: RuleGenerationParams -> StatefulRandom Rule
generateRule p = do
  isFindRoot <- generateRandom
  ep <- generatePredicate p
  cp <- generatePredicate p
  sd <- generateRandom
  sdd <- generateRandomMax (maxDistance p)
  dr <- generateRandomMax (maxDependencyRelationsIndex p)
  if isFindRoot
    then return (FindRoot ep)
    else return (FindLink ep cp sd sdd dr)

generatePredicate :: RuleGenerationParams -> StatefulRandom Predicate
generatePredicate p = do
  tag <- generateRandomMax (maxTagIndex p)
  fps <- generateFeaturePairs p
  return $ Predicate tag (map (fmap Just) fps)

generateFeaturePairs :: RuleGenerationParams -> StatefulRandom [(Int, Int)]
generateFeaturePairs p = do
  size <- generateRandomMax (maxFeaturePairs p)
  fns <- replicateM size $ generateRandomMax (maxFeatureNameIndex p)
  fvs <- replicateM size $ generateRandomMax (maxFeatureValuesIndex p)
  return $ zip fns fvs

mutatePredicate :: RuleGenerationParams -> Predicate -> StatefulRandom Predicate
mutatePredicate p pr = do
  op <- generateRandom
  n <- generateRandomMax $ length (predicateFeaturePairs pr) - 1
  case op of
    MutatePredicateTag -> do
      tag <- generateRandomMax (maxTagIndex p)
      return $ pr {predicateTag = tag}
    DeletePredicateFeaturePair -> do
      return $ pr {predicateFeaturePairs = deleteN n (predicateFeaturePairs pr)}
    MutatePredicateFeaturePair -> do
      fn <- generateRandomMax (maxFeatureNameIndex p)
      fv <- generateRandomMax (maxFeatureValuesIndex p)
      return $ pr {predicateFeaturePairs = replaceN n (fn, Just fv) (predicateFeaturePairs pr)}
    AddPredicateFeaturePair -> do
      fn <- generateRandomMax (maxFeatureNameIndex p)
      fv <- generateRandomMax (maxFeatureValuesIndex p)
      return $ pr {predicateFeaturePairs = (fn, Just fv) : predicateFeaturePairs pr}

mutateRule :: RuleGenerationParams -> Rule -> StatefulRandom Rule
mutateRule p (FindRoot pr) = do
  op <- generateRandom
  case op of
    ConvertRootToLink -> do
      cp <- generatePredicate p
      sd <- generateRandom
      sdd <- generateRandomMax (maxDistance p)
      dr <- generateRandomMax (maxDependencyRelationsIndex p)
      return $ FindLink pr cp sd sdd dr
    MutateRootPredicate -> do
      pr' <- mutatePredicate p pr
      return $ FindRoot pr'
mutateRule p (FindLink pr cp sd sdd dr) = do
  op <- generateRandom
  case op of
    ConvertLinkToRoot -> return $ FindRoot pr
    MutateLinkPredicate -> do
      pr' <- mutatePredicate p pr
      return $ FindLink pr' cp sd sdd dr
    MutateCorrespondentPredicate -> do
      cp' <- mutatePredicate p cp
      return $ FindLink pr cp' sd sdd dr
    MutateSearchDirection -> do
      sd' <- generateRandom
      return $ FindLink pr cp sd' sdd dr
    MutateSearchDistance -> do
      sdd' <- generateRandomMax (maxDistance p)
      return $ FindLink pr cp sd sdd' dr
    MutateDependencyRelation -> do
      dr' <- generateRandomMax (maxDependencyRelationsIndex p)
      return $ FindLink pr cp sd sdd dr'

mutateRuleSet :: RuleGenerationParams -> RuleSet -> StatefulRandom RuleSet
mutateRuleSet p (RuleSet rs) =
  if null rs
    then return (RuleSet rs)
    else do
      op <- generateRandom
      n <- generateRandomMax $ length rs - 1
      go op n
  where
    go op n = do
      case op of
        DeleteRule -> return $ RuleSet $ deleteN n rs
        InsertRule -> do
          newRule <- generateRule p
          return $ RuleSet $ replaceN n newRule rs
        MutateRule -> do
          mutatedRule <- mutateRule p (rs !! n)
          return $ RuleSet $ replaceN n mutatedRule rs

crossover2RuleSets :: RuleSet -> RuleSet -> StatefulRandom RuleSet
crossover2RuleSets (RuleSet rs1) (RuleSet rs2) = do
  n <- generateRandomMax (min (length rs1) (length rs2))
  return $ RuleSet $ take n rs1 ++ drop n rs2
