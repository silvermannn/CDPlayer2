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

data MutationRuleSet
  = DeleteRule
  | InsertRule
  | MutateRule
  deriving (Show, Enum, Bounded)

data MutationRule
  = Convert
  | MutateExactTag
  | DeleteExactFeaturePair
  | AddExactFeaturePair
  | MutateExactFeaturePair
  | DeleteCorrespondentFeaturePair
  | MutateCorrespondentTag
  | MutateCorrespondentFeaturePair
  | MutateCorrespondentFeaturePairSetValueNothing
  | MutateCorrespondentFeaturePairWithValue
  | MutateCorrespondentFeaturePairWithValueNothing
  | MutateCorrespondentSearchDirection
  | MutateCorrespondentDependencyRelation
  deriving (Show, Enum, Bounded)

generateRandomRuleSet :: RandomGen g => RuleGenerationParams -> g -> Int -> (RuleSet, g)
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
    (n1, t) = n `divMod` (tagsSize p)
    ps = int2Featurepairs p n1

int2CorrespondentPredicate :: RuleGenerationParams -> Int -> CorrespondentPredicate
int2CorrespondentPredicate p n = CorrespondentPredicate (sd sdd) t (map (fmap Just) ps)
  where
    (n1, [t, sdi, sdd]) = generateMods n [dependencyRelationsSize p, 2, maxDistance p]
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
generateMods n xs = foldr (\x (n', acc) -> (n' `div` x, (n' `mod` x) : acc)) (n, []) xs

mutateRule :: RuleGenerationParams -> Rule -> Int -> Rule
mutateRule p r n =
  case r of
    (FindRoot ep@(ExactPredicate t fps)) ->
      case toEnum (op `mod` 6) of
        Convert -> FindLink ep (int2CorrespondentPredicate p n'') dr'
        MutateExactTag -> FindRoot (ExactPredicate t' fps)
        DeleteExactFeaturePair -> FindRoot (ExactPredicate t (fps1 ++ fps2))
        AddExactFeaturePair -> FindRoot (ExactPredicate t (fps1 ++ (fn, fv) : fp : fps2))
        MutateExactFeaturePair -> FindRoot (ExactPredicate t (fps1 ++ (fn, fv) : fps2))
        _ -> error "Cannot mutate"
      where
        (fps1, fp:fps2) = splitAt i fps
        (n'', i) = n' `divMod` (length fps)
    (FindLink ep@(ExactPredicate t fps) cp@(CorrespondentPredicate sd t2 fpsc) dr) ->
      case toEnum op of
        Convert -> FindRoot ep
        MutateExactTag -> FindLink (ExactPredicate t' fps) cp dr
        DeleteExactFeaturePair -> FindLink (ExactPredicate t (fps1 ++ fps2)) cp dr
        AddExactFeaturePair -> FindLink (ExactPredicate t (fps1 ++ (fn, fv) : fp : fps2)) cp dr
        MutateExactFeaturePair -> FindLink (ExactPredicate t (fps1 ++ (fn, fv) : fps2)) cp dr
        MutateCorrespondentTag -> FindLink ep (CorrespondentPredicate sd t' fpsc) dr
        DeleteCorrespondentFeaturePair ->
          FindLink ep (CorrespondentPredicate sd t2 (fps21 ++ fps22)) dr
        MutateCorrespondentFeaturePair ->
          FindLink ep (CorrespondentPredicate sd t2 (fps21 ++ (fn, Just fv) : fps22)) dr
        MutateCorrespondentFeaturePairSetValueNothing ->
          FindLink ep (CorrespondentPredicate sd t2 (fps21 ++ (fn, Nothing) : fps22)) dr
        MutateCorrespondentFeaturePairWithValue ->
          FindLink ep (CorrespondentPredicate sd t2 (fps21 ++ (fn2, Just fv) : fps22)) dr
        MutateCorrespondentFeaturePairWithValueNothing ->
          FindLink ep (CorrespondentPredicate sd t2 (fps21 ++ (fn2, Nothing) : fps22)) dr
        MutateCorrespondentSearchDirection ->
          FindLink
            ep
            (CorrespondentPredicate
               (if sd' == 0
                  then SearchLeft sdd
                  else SearchRight sdd)
               t2
               fpsc)
            dr
        MutateCorrespondentDependencyRelation -> FindLink ep (CorrespondentPredicate sd t2 fpsc) dr'
      where
        (fps1, fp:fps2) = splitAt i fps
        (fps21, (fn2, _):fps22) = splitAt i fpsc
        (n'', i) = n' `divMod` (length fps)
  where
    (n', [op, t', dr', fn, fv, sd', sdd]) =
      generateMods
        n
        [ 1 + fromEnum (maxBound :: MutationRule)
        , tagsSize p
        , dependencyRelationsSize p
        , featureNamesSize p
        , featureValuesSize p
        , 2
        , maxDistance p
        ]

mutateRuleSet :: RandomGen g => RuleGenerationParams -> g -> RuleSet -> (RuleSet, g)
mutateRuleSet p g (RuleSet rs) =
  ( case toEnum op of
      DeleteRule -> RuleSet (rs1 ++ rs2)
      InsertRule -> RuleSet (rs1 ++ int2Rule p n' : r : rs2)
      MutateRule -> RuleSet (rs1 ++ mutateRule p r n' : rs2)
  , g')
  where
    (n, g') = uniform g
    (n', [op, i]) = generateMods n [1 + fromEnum (maxBound :: MutationRuleSet), (length rs - 1)]
    (rs1, r:rs2) = splitAt i rs

crossover2RuleSets :: RandomGen g => g -> RuleSet -> RuleSet -> (RuleSet, g)
crossover2RuleSets g (RuleSet rs1) (RuleSet rs2) = (RuleSet $ take n' rs1 ++ drop n' rs2, g')
  where
    (n, g') = uniform g
    n' = n `mod` (min (length rs1) (length rs2))
