module Data.Syntax.Rule where

import GHC.Generics

import System.Random

import Data.Syntax.DependencyRelation
import Data.Syntax.DependencyTree
import Data.Syntax.Sentence
import Data.Syntax.Tag

data SearchDirection
  = SearchLeft
  | SearchRight
  deriving (Show, Generic, Finite, Uniform)

data Predicate = Predicate
  { predicateTag :: Int
  , predicateFeaturePairs :: [(Int, Maybe Int)]
  } deriving (Show)

data Rule
  = FindRoot Predicate
  | FindLink Predicate Predicate SearchDirection Int DependencyRelation
  deriving (Show)

newtype RuleSet =
  RuleSet [Rule]
  deriving (Show)

data Result =
  Result DependencyTree Sentence
  deriving (Show)
