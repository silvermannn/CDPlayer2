module Data.Syntax.Rule where

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Tag

data SearchDirection
  = SearchLeft Int
  | SearchRight Int
  deriving (Show)

data ExactPredicate =
  ExactPredicate Int [(Int, Int)]
  deriving (Show)

data CorrespondentPredicate =
  CorrespondentPredicate SearchDirection Int [(Int, Maybe Int)]
  deriving (Show)

data Rule
  = FindRoot ExactPredicate
  | FindLink ExactPredicate CorrespondentPredicate DependencyRelation
  deriving (Show)

newtype RuleSet =
  RuleSet [Rule]
  deriving (Show)

data Result =
  Result DependencyTree Sentence
  deriving (Show)
