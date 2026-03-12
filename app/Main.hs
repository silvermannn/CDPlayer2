module Main
  ( main
  ) where

import System.Random

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence
import Data.Syntax.Tag

testWords = [SWord i i i [(1, 0), (5, 0)] | i <- [0 .. 10]]

testSentence = Sentence testWords

testRule1 = FindRoot (ExactPredicate 2 [])

testRule2 =
  FindLink
    (ExactPredicate 2 [])
    (CorrespondentPredicate (SearchRight 10) 3 [])
    100

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
  mapM_ showDependencyTree
    $ map (\(Result a _) -> a)
    $ parseSentence (RuleSet rs1) testSentence
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
    (RuleSet rs1, g1) = generateRandomRuleSet params g 10
    (RuleSet rs2, g2) = generateRandomRuleSet params g1 15
    (RuleSet rs3, g3) = crossover2RuleSets g2 (RuleSet rs1) (RuleSet rs2)
    (RuleSet rs4, g4) = mutateRuleSet params g3 (RuleSet rs3)
    (RuleSet rs5, g5) = mutateRuleSet params g4 (RuleSet rs4)
    (RuleSet rs6, g6) = mutateRuleSet params g5 (RuleSet rs5)
