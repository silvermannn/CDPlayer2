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
import Data.Syntax.Rule.Evolution
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence
import Data.Syntax.Tag

testWords = [SWord i i i [] | i <- [0 .. 19]]

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

evolParams =
  EvolutionParameters
    { maxPopulationSize = 100
    , maxRulesetSize = 30
    , mutationRate = 0.1
    , survivalRate = 0.5
    , generationParams = params
    }

main :: IO ()
main = do
  print testSentence
  print evolParams
  
  print "best rule 1"
  mapM_ print rs2
  showDependencyTree $ last $ map (\(Result a _) -> a) $ parseSentence (RuleSet rs2) testSentence
  print "best rule 2"
  print (rss2 == rss3)
  print (rs2 == rs3)
  mapM_ print rs3
  showDependencyTree $ last $ map (\(Result a _) -> a) $ parseSentence (RuleSet rs3) testSentence
  print ":DONE!"
  where
    g = mkStdGen 31337
    (ps1, g1) = runState (generateInitialPopulation evolParams) g
    (ps2, g2) = runState (evolutionStep evolParams emptyDependencyTree testSentence ps1) g1
    (Population rss2) = ps2
    (RuleSet rs2) = head rss2
    (evol, g3) = runState (infiniteEvolution evolParams emptyDependencyTree testSentence ps2) g2
    ps3 = last $ take 55555 evol
    (Population rss3) = ps3
    (RuleSet rs3) = head rss3
