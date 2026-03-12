module Main
  ( main
  ) where

import System.Random

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Tag
import Data.Syntax.Rule
import Data.Syntax.Rule.Application
import Data.Syntax.Rule.Random

testWords = [SWord i i i [] | i <- [1 .. 10]]

testSentence = Sentence testWords

empty = emptyDependencyTree

testRule1 = FindRoot (ExactPredicate 2 [])

testRule2 =
  FindLink
    (ExactPredicate 2 [])
    (CorrespondentPredicate (SearchRight 10) 3 [])
    100

testRules = RuleSet [testRule1, testRule2]

params = RuleGenerationParams {maxDistance = 10, tagsSize = 20, maxFeaturePairs = 5, featureNamesSize = 10, featureValuesSize = 10, dependencyRelationsSize = 10}

main :: IO ()
main = do
  showDependencyTree empty
  mapM_ showDependencyTree added
  mapM_ showDependencyTree
    $ map (\(Result a _) -> a)
    $ applyRule testRule2 (Result (head added) testSentence)
  mapM_ showDependencyTree
    $ map (\(Result a _) -> a)
    $ cyclicApplication testRules (Result empty testSentence)

  mapM_ print rs    
  print ":DONE!"
  where
    g = mkStdGen 42
    (RuleSet rs, g') = generateRandomRuleSet params g 10 
    added =
      map (\(Result a _) -> a) $ applyRule testRule1 (Result empty testSentence)
