module Main
  ( main
  ) where

import System.Random

import Control.Monad.State

import Data.List

import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Evolution
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence
import Data.Syntax.Tag

testWords :: [TaggedWord]
testWords = [SWord i i i [(0, 0)] | i <- [0 .. 9]]

testSentence :: Sentence
testSentence = Sentence testWords

params :: RuleGenerationParams
params =
  RuleGenerationParams
    { maxDistance = 10
    , maxTagIndex = 10
    , maxFeaturePairs = 2
    , maxFeatureNameIndex = 10
    , maxFeatureValuesIndex = 10
    , maxDependencyRelationsIndex = 10
    }

evolParams :: EvolutionParameters
evolParams =
  EvolutionParameters
    { maxPopulationSize = 1000
    , maxRulesetSize = 50
    , mutationRate = 0.1
    , survivalRate = 0.5
    , generationParams = params
    }

main :: IO ()
main = do
  print testSentence
  print evolParams
  print "best rule 1"
  mapM_ print $ sort rs2
  print a
  showDependencyTree a
  print "best rule 2"
  mapM_ print rs3
  showDependencyTree $ ($) (\(Result b _) -> b) $ last $ parseSentence (RuleSet rs3) testSentence
  where
    (Result a _) = last $ parseSentence (RuleSet rs2) testSentence
    g = mkStdGen 31337
    (ps1, g1) = runState (generateInitialPopulation evolParams) g
    (ps2, g2) = runState (evolutionStep evolParams emptyDependencyTree testSentence ps1) g1
    (Population ((RuleSet rs2):_)) = ps2
    (evol, _) = runState (infiniteEvolution evolParams emptyDependencyTree testSentence ps2) g2
    ps3 = last $ take 100 evol
    (Population ((RuleSet rs3):_)) = ps3
