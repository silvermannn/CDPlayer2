module Main
  ( main
  ) where

import System.Random

import Control.Monad
import Control.Monad.State

import Data.List
import qualified Data.Map as M

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Evolution
import Data.Syntax.Rule.Predicate
import Data.Syntax.Rule.Random
import Data.Syntax.Sentence
import Data.Syntax.Tag
import Data.Tree
import Data.TreeSearch

testWords = [SWord i i i [(0, 0)] | i <- [0 .. 9]]

testSentence = Sentence testWords

testRule1 = FindRoot (Predicate 2 [])

testRule2 = FindLink (Predicate 2 []) (Predicate 3 []) SearchRight 10 100

testRules = RuleSet [testRule1, testRule2]

params =
  RuleGenerationParams
    { maxDistance = 10
    , maxTagIndex = 10
    , maxFeaturePairs = 2
    , maxFeatureNameIndex = 10
    , maxFeatureValuesIndex = 10
    , maxDependencyRelationsIndex = 10
    }

evolParams =
  EvolutionParameters
    { maxPopulationSize = 1000
    , maxRulesetSize = 50
    , mutationRate = 0.1
    , survivalRate = 0.5
    , generationParams = params
    }

testLT1 :: FastTree String
testLT1 = singletonLT "item 1"

testLT2 = insertLT "item 4" 1 $ insertLT "item 3" 0 $ insertLT "item 2" 0 testLT1

testTrees = toTreeLT testLT2

main :: IO ()
main = do
  print testLT2
  print $ lastAddedLT testLT2
  mapM_ (putStrLn . drawTree) testTrees
  print testSentence
  print evolParams
  print "best rule 1"
  mapM_ print $ sort rs2
  print a
  showDependencyTree a
  print "best rule 2"
  print (rss2 == rss3)
  print (rs2 == rs3)
  mapM_ print rs3
  showDependencyTree $ ($) (\(Result a _) -> a) $ last $ parseSentence (RuleSet rs3) testSentence
  print ":DONE!"
  where
    (Result a _) = last $ parseSentence (RuleSet rs2) testSentence
    g = mkStdGen 31337
    (ps1, g1) = runState (generateInitialPopulation evolParams) g
    (ps2, g2) = runState (evolutionStep evolParams emptyDependencyTree testSentence ps1) g1
    (Population rss2) = ps2
    (RuleSet rs2) = head rss2
    (evol, g3) = runState (infiniteEvolution evolParams emptyDependencyTree testSentence ps2) g2
    ps3 = last $ take 100 evol
    (Population rss3) = ps3
    (RuleSet rs3) = head rss3
