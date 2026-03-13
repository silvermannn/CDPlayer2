module Main
  ( main
  ) where

import System.Random

import Control.Monad
import Control.Monad.State

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence
import Data.Syntax.Tag

testWords = [SWord i i i [(1, 0), (5, 0)] | i <- [0 .. 10]]

testSentence = Sentence testWords

testRule1 = FindRoot (Predicate 2 [])

testRule2 = FindLink (Predicate 2 []) (Predicate 3 []) SearchRight 10 100

testRules = RuleSet [testRule1, testRule2]

params =
  RuleGenerationParams
    { maxDistance = 10
    , tagsSize = 20
    , maxFeaturePairs = 5
    , featureNamesSize = 10
    , featureValuesSize = 10
    , dependencyRelationsSize = 10
    }

main :: IO ()
main = do
  print testSentence
  print params
  mapM_ showDependencyTree $ map (\(Result a _) -> a) $ parseSentence (RuleSet rs1) testSentence
  print "--- Random 1"
  mapM_ print $ rs1
  print "--- Random 2"
  mapM_ print $ rs2
  print "--- Crossover"
  mapM_ print $ rs3
  print "--- Mutated 1"
  mapM_ print $ rs4
  print "--- Mutated 2"
  mapM_ print $ rs5
  print "--- Mutated 3"
  mapM_ print $ rs6
  print ":DONE!"
  where
    g = mkStdGen 31337
    ([RuleSet rs1, RuleSet rs2], g2) = runState (replicateM 2 (generateRuleSet params 10)) g
    (RuleSet rs3, g3) = runState (crossover2RuleSets (RuleSet rs1) (RuleSet rs2)) g2
    ([RuleSet rs4, RuleSet rs5, RuleSet rs6], g6) =
      runState (replicateM 3 $ mutateRuleSet params (RuleSet rs3)) g3
