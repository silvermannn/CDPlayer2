module Data.Syntax.Rule.Random where

import System.Random

import Control.Monad

import Data.List

import GHC.Generics

import Data.Random.Stateful
import Data.Syntax.Rule

data RuleGenerationParams = RuleGenerationParams
  { maxDistance :: Int
  , tagsSize :: Int
  , maxFeaturePairs :: Int
  , featureNamesSize :: Int
  , featureValuesSize :: Int
  , dependencyRelationsSize :: Int
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
  return $ RuleSet rs

generateRule :: RuleGenerationParams -> StatefulRandom Rule
generateRule p = do
  isFindRoot <- generateRandom
  ep <- generatePredicate p
  cp <- generatePredicate p
  sd <- generateRandom
  sdd <- generateRandomMax (maxDistance p)
  dr <- generateRandomMax (dependencyRelationsSize p)
  if isFindRoot
    then return (FindRoot ep)
    else return (FindLink ep cp sd sdd dr)

generatePredicate :: RuleGenerationParams -> StatefulRandom Predicate
generatePredicate p = do
  tag <- generateRandomMax (tagsSize p)
  fps <- generateFeaturePairs p
  return $ Predicate tag (map (fmap Just) fps)

generateFeaturePairs :: RuleGenerationParams -> StatefulRandom [(Int, Int)]
generateFeaturePairs p = do
  size <- generateRandomMax (maxFeaturePairs p)
  fns <- replicateM size $ generateRandomMax (featureNamesSize p)
  fvs <- replicateM size $ generateRandomMax (featureValuesSize p)
  return $ zip fns fvs

mutatePredicate :: RuleGenerationParams -> Predicate -> StatefulRandom Predicate
mutatePredicate p pr = do
  op <- generateRandom
  n <- generateRandomMax $ length (predicateFeaturePairs pr) - 1
  let (fps1, fp:fps2) = splitAt n (predicateFeaturePairs pr)
   in case op of
        MutatePredicateTag -> do
          tag <- generateRandomMax (tagsSize p)
          return $ pr {predicateTag = tag}
        DeletePredicateFeaturePair -> do
          return $ pr {predicateFeaturePairs = fps1 ++ fps2}
        MutatePredicateFeaturePair -> do
          fn <- generateRandomMax (featureNamesSize p)
          fv <- generateRandomMax (featureValuesSize p)
          return $ pr {predicateFeaturePairs = fps1 ++ (fn, Just fv) : fps2}
        AddPredicateFeaturePair -> do
          fn <- generateRandomMax (featureNamesSize p)
          fv <- generateRandomMax (featureValuesSize p)
          return $ pr {predicateFeaturePairs = (fn, Just fv) : predicateFeaturePairs pr}

mutateRule :: RuleGenerationParams -> Rule -> StatefulRandom Rule
mutateRule p (FindRoot pr) = do
  op <- generateRandom
  case op of
    ConvertRootToLink -> do
      cp <- generatePredicate p
      sd <- generateRandom
      sdd <- generateRandomMax (maxDistance p)
      dr <- generateRandomMax (dependencyRelationsSize p)
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
      dr' <- generateRandomMax (dependencyRelationsSize p)
      return $ FindLink pr cp sd sdd dr'

mutateRuleSet :: RuleGenerationParams -> RuleSet -> StatefulRandom RuleSet
mutateRuleSet p (RuleSet rs) = do
  op <- generateRandom
  n <- generateRandomMax $ length rs - 1
  go p op n rs
  where
    go p op n rs = do
      case op of
        DeleteRule -> return $ RuleSet (rs1 ++ rs2)
        InsertRule -> do
          newRule <- generateRule p
          return $ RuleSet (rs1 ++ newRule : rs2)
        MutateRule -> do
          mutatedRule <- mutateRule p r
          return $ RuleSet (rs1 ++ mutatedRule : rs2)
      where
        (rs1, r:rs2) = splitAt n rs

crossover2RuleSets :: RuleSet -> RuleSet -> StatefulRandom RuleSet
crossover2RuleSets (RuleSet rs1) (RuleSet rs2) = do
  n <- generateRandomMax (min (length rs1) (length rs2))
  return $ RuleSet $ take n rs1 ++ drop n rs2
