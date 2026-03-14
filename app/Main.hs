module Main
  ( main
  ) where

import System.Random

import Control.Monad
import Control.Monad.State

import Data.Bifunctor
import qualified Data.Map as M

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Evolution
import Data.Syntax.Rule.Random
import Data.Syntax.Rule.Predicate
import Data.Syntax.Sentence
import Data.Syntax.Tag
import Data.Tree
import Data.TreeSearch

testTree = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 1 [Node 11 []], Node 10 []]]

testWords = [SWord i i i [] | i <- [0 .. 9]]

testSentence = Sentence testWords

testRule1 = FindRoot (Predicate 2 [])

testRule2 = FindLink (Predicate 2 []) (Predicate 3 []) SearchRight 10 100

testRules = RuleSet [testRule1, testRule2]

params =
  RuleGenerationParams
    { maxDistance = 10
    , tagsSize = 10
    , maxFeaturePairs = 0
    , featureNamesSize = 1
    , featureValuesSize = 1
    , dependencyRelationsSize = 10
    }

evolParams =
  EvolutionParameters
    { maxPopulationSize = 100
    , maxRulesetSize = 15
    , mutationRate = 0.1
    , survivalRate = 0.5
    , generationParams = params
    }

ps = [10, 20, 30]

testLT1 :: LinearTree Int Int
testLT1 = singletonLT 10 1

testLT2 = insertLT 4 1 20 $ insertLT 3 0 20 $ insertLT 2 0 10 testLT1

testTrees = toTreeLT testLT2

main :: IO ()
main = do
  print testLT2
  mapM_ print $ map (`cachedItemsLT` testLT2) ps
  mapM_ (putStrLn . drawTree) testTrees 

  print testSentence
  print evolParams
  print "best rule 1"
  mapM_ print rs2
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
    ps3 = last $ take 5 evol
    (Population rss3) = ps3
    (RuleSet rs3) = head rss3
