module Main
  ( main
  ) where

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Tag
import Data.Syntax.TreeBuildRule

testWords = [SWord i i i [] | i <- [1 .. 10]]

testSentence = Sentence testWords

empty = emptyDependencyTree

testRule1 = FindRoot (ExactPredicate 2 [])

testRule2 =
  FindLink
    (ExactPredicate 2 [])
    (CorrespondentPredicate (SearchRight 10) 3 [])
    100

main :: IO ()
main = do
  showDependencyTree empty
  mapM_ showDependencyTree added
  mapM_ showDependencyTree
    $ map (\(Result a _) -> a)
    $ applyRule testRule2 (Result (head added) testSentence)
  where
    added =
      map (\(Result a _) -> a) $ applyRule testRule1 (Result empty testSentence)
