module Main
  ( main
  ) where

import System.Random

import Control.Monad.State

import Data.List

import Data.FastTree
import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Evolution
import Data.Syntax.Rule.Predicate
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence
import Data.Syntax.Tag

testWords :: [TaggedWord]
testWords = [TaggedWord i i (i `mod` 2) [] | i <- [0, 1, 2, 3]]

testTree :: DependencyTree
testTree =
  DependencyTree
    $ Just
    $ foldr (\w t -> insertLT (w, 1) 0 t) (singletonLT (TaggedWord 0 0 0 [], getRootRelation))
    $ drop 1 testWords

testSentence :: Sentence
testSentence = newSentence testWords

params :: RuleGenerationParams
params =
  RuleGenerationParams
    { maxDistance = 10
    , maxTagIndex = 3
    , maxFeaturePairs = 0
    , maxFeatureNameIndex = 19
    , maxFeatureValuesIndex = 19
    , maxDependencyRelationsIndex = 3
    }

evolParams :: EvolutionParameters
evolParams =
  EvolutionParameters
    { maxPopulationSize = 1000
    , maxRulesetSize = 10
    , mutationRate = 0.01
    , survivalRate = 0.5
    , generationParams = params
    }

testRuleSet =
  RuleSet
    [ FindRoot (Predicate {predicateTag = 0, predicateFeaturePairs = []})
    , FindRoot (Predicate {predicateTag = 1, predicateFeaturePairs = []})
    , FindLink
        (Predicate {predicateTag = 0, predicateFeaturePairs = []})
        (Predicate {predicateTag = 0, predicateFeaturePairs = []})
        SearchRight
        4
        1
    , FindLink
        (Predicate {predicateTag = 0, predicateFeaturePairs = []})
        (Predicate {predicateTag = 1, predicateFeaturePairs = []})
        SearchRight
        4
        1
    , FindLink
        (Predicate {predicateTag = 0, predicateFeaturePairs = []})
        (Predicate {predicateTag = 2, predicateFeaturePairs = []})
        SearchRight
        4
        1
    ]

main :: IO ()
main = do
  print testSentence
  print testTree
  showDependencyTree testTree
  showDependencyTree $ updateFringe testTree
  print $ calcDependancyTreeDifference testTree testTree
  print testRuleSet
  mapM_ (\a -> showDependencyTree a >> print (calcDependancyTreeDifference a testTree))
    $ map (\(Result a _) -> a)
    $ parseSentence testRuleSet testSentence
  print evolParams
  print "best rule 1"
  mapM_ print rs2
  mapM_ (\a -> showDependencyTree a >> print (calcDependancyTreeDifference a testTree))
    $ map (\(Result a _) -> a)
    $ parseSentence (RuleSet rs2) testSentence
  print "best rule 2"
  mapM_ print rs3
  mapM_ (\a -> showDependencyTree a >> print (calcDependancyTreeDifference a testTree))
    $ map (\(Result a _) -> a)
    $ parseSentence (RuleSet rs3) testSentence
  where
    g = mkStdGen 31337
    (ps1, g1) = runState (generateInitialPopulation evolParams) g
    (ps2, g2) = runState (evolutionStep evolParams testTree testSentence ps1) g1
    (Population ((RuleSet rs2):_)) = ps2
    (evol, _) = runState (infiniteEvolution evolParams testTree testSentence ps2) g2
    ps3 = last $ take 2000 evol
    (Population ((RuleSet rs3):_)) = ps3
