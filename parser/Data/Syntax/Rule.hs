module Data.Syntax.Rule where

import GHC.Generics

import System.Random

import Data.Syntax.DependencyRelation
import Data.Syntax.Rule.Predicate

data SearchDirection
  = SearchLeft
  | SearchRight
  deriving (Show, Eq, Ord, Generic, Finite, Uniform)

data Rule
  = FindRoot Predicate
  | FindLink Predicate Predicate SearchDirection Int DependencyRelation
  deriving (Show, Eq, Ord)

newtype RuleSet =
  RuleSet [Rule]
  deriving (Show, Eq)
